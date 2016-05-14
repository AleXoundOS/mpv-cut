{-# LANGUAGE CPP #-} -- only for GHCi to avoid mess with foreign export objects
{-# LANGUAGE TemplateHaskell #-} -- only for embedFile
{-# LANGUAGE OverloadedStrings #-} -- ByteStrings
-- |
-- Module      : MPV_Cut
-- Copyright   : (c) 2016 Alexander Tomokhov
--
-- License     : GPL3
-- Maintainer  : alexoundos@ya.ru
-- Stability   : experimental
-- Portability : GHC

module MPV_Cut where
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import GHC.IO.Handle
import System.Posix.IO (fdToHandle, dup)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
  (ByteString, useAsCString, packCString, cons, hGetContents)
import Data.List ((\\), find, sort)
import Text.Read (readMaybe)
import Data.FileEmbed (embedFile)
import Data.ByteString.Lazy.Search as BSLS (replace, breakAfter)

import Debug.Trace
myTrace :: Show b => b -> b
myTrace x = trace ("\ntrace: " ++ show x) x


-- which side of piece: start | end | act as both | file start | file end
data Side = A | B | X | S | E
    deriving (Eq, Ord, Show, Read)

data TimeStamp = TimeStamp Side BSL.ByteString
    deriving (Show)
instance Eq TimeStamp where
    (TimeStamp _ str1) == (TimeStamp _ str2)
      = (unsafeReadDouble str1) == (unsafeReadDouble str2)
instance Ord TimeStamp where
    (TimeStamp _ str1) < (TimeStamp _ str2)
      = (unsafeReadDouble str1) < (unsafeReadDouble str2)
    (TimeStamp _ str1) > (TimeStamp _ str2)
      = (unsafeReadDouble str1) > (unsafeReadDouble str2)
    (TimeStamp _ str1) <= (TimeStamp _ str2)
      = (unsafeReadDouble str1) <= (unsafeReadDouble str2)
    (TimeStamp _ str1) >= (TimeStamp _ str2)
      = (unsafeReadDouble str1) >= (unsafeReadDouble str2)

getTimeStampSide :: TimeStamp -> Side
getTimeStampSide (TimeStamp side _) = side

getTimeStampStr :: TimeStamp -> BSL.ByteString
getTimeStampStr (TimeStamp _ str) = str

getTimeStampDouble :: TimeStamp -> CDouble
getTimeStampDouble (TimeStamp _ str) = unsafeReadDouble str

scriptVersion :: BSL.ByteString
scriptVersion = "0.1" -- bash script format version

scriptTemplateFile :: BSL.ByteString
scriptTemplateFile = BSL.fromStrict $(embedFile "script_template.sh")

outFileExtension :: BSL.ByteString
outFileExtension = "mkv"

-- this is a Piece of stream, i.e. A-B, A-X, A-E, S-B, S-X, S-E
newtype Piece = Piece (TimeStamp,TimeStamp)
    deriving (Eq, Ord, Show)

newtype ScriptData = ScriptData ( BSL.ByteString, BSL.ByteString, BSL.ByteString
                                , [Piece] )
    deriving (Eq, Ord, Show)

foreign import ccall unsafe "stdlib.h atof" c_atof :: CString -> IO CDouble

unsafeReadDouble :: BSL.ByteString -> CDouble
unsafeReadDouble = let toCString = BS.useAsCString . BSL.toStrict
                   in unsafePerformIO . flip toCString c_atof

foreign import ccall "stdio.h fileno" fileno :: Ptr CFile -> IO CInt

fpToHandle :: Ptr CFile -> IO Handle
fpToHandle fp = do
    int <- fileno fp
    -- get a duplicate of original file descriptor
    -- with combination of hClose avoids problem with locked files after write
    fd <- dup (fromIntegral int)
    handle <- fdToHandle fd
    return handle

#ifndef GHCI
foreign export ccall h_add :: Ptr CFile -> CString -> Char -> CString -> IO CInt
#endif
h_add :: Ptr CFile -> CString -> Char -> CString -> IO CInt
h_add fp cMediaFilename char str = do
    let maybeSide :: Maybe Side
        maybeSide = readMaybe [char]
    if maybeSide == Nothing
        then do
            return 1
        else do
            -- | fetching parameters for add
            mediaFilenameBind <- BS.packCString cMediaFilename
            let mediaFileName = BSL.fromStrict mediaFilenameBind

            let side = (\(Just x) -> x) maybeSide

            timeBind <- BS.packCString str
            let time = BSL.fromStrict timeBind

            h <- fpToHandle fp

            -- read using duplicate Handle - avoid semi-closed Handle afterwards
            h2 <- hDuplicate h
            hSeek h2 AbsoluteSeek 0
            inpFileContentsStrict <- BS.hGetContents h2
            let inpFileContents = BSL.fromStrict inpFileContentsStrict

            -- processing and writing
            hSeek h AbsoluteSeek 0
            BSL.hPutStr h
              $ add inpFileContents mediaFileName (TimeStamp side time)

            hClose h2
            hClose h
            return 0

nameFromFilename :: BSL.ByteString -> BSL.ByteString
nameFromFilename filename
  = BSL.reverse $ BSL.drop 1 $ BSL.dropWhile (/= '.') $ (BSL.reverse filename)

-- get closest A/B TimeStamp from the supplied list against specific time
-- A is searched backward, B is searched forward
closest :: Side -> CDouble -> [TimeStamp] -> Maybe TimeStamp
closest s d ts
    | s == A = find ((== A) . getTimeStampSide) (reverse tsBackward)
    | s == B = find ((== B) . getTimeStampSide) tsForward
    | otherwise = error "unexpected side in closest"
  where
    st = sort ts
    tsForward =  dropWhile (( <= d ) . getTimeStampDouble) st
    tsBackward = takeWhile ((  < d ) . getTimeStampDouble) st

adoptees :: [TimeStamp] -> [TimeStamp] -> [Piece]
adoptees remaining ts = foldr f [] remaining
  where
    f tA@(TimeStamp A s) acc = case (closest B (unsafeReadDouble s) ts) of
        Just tB -> Piece (tA, tB) : acc
        Nothing -> acc
    f tB@(TimeStamp B s) acc = case (closest A (unsafeReadDouble s) ts) of
        Just tA -> Piece (tA, tB) : acc
        Nothing -> acc
    f _ _ = error "unexpected side in adoptees"

borderTheSide :: TimeStamp -> Piece
borderTheSide t | getTimeStampSide t == A = Piece (t, TimeStamp E "null")
                | getTimeStampSide t == B = Piece (TimeStamp S "null", t)
                | otherwise = error "unexpected side in borderTheSide"

-- straight A-B from bottom to top level ++ closest A/B ++ bordered
allPieces :: [TimeStamp] -> [Piece]
allPieces ts =
    let sts = sort ts
        lNatives = nativeCitizens sts
        lAdoptees = adoptees (sts \\ piecesToTs lNatives) sts
        -- remaining = foldl (\\) ts (map piecesToTs [lNatives, lAdoptees])
        remaining = foldr ((flip (\\)) . piecesToTs) sts [lNatives, lAdoptees]
        lBordered = map borderTheSide remaining
    in sort $ lNatives ++ lAdoptees ++ lBordered

piecesToTs :: [Piece] -> [TimeStamp]
piecesToTs (Piece (a,b) : xs) = a : b : piecesToTs xs
piecesToTs _          = []

-- all straight A-B from bottom to top
nativeCitizens :: [TimeStamp] -> [Piece]
nativeCitizens ts =
    let pieces = firstCitizens ts
    in if not . null $ pieces
       then nativeCitizens (ts \\ piecesToTs pieces) ++ pieces
       else []
    -- maybe concatMap usage here

-- gets first straight A-B pieces, picking them out of the whole list
firstCitizens :: [TimeStamp] -> [Piece]
firstCitizens (x:y:xs) =
    if (getTimeStampSide x) `elem` [A,X] && (getTimeStampSide y) `elem` [B,X]
    then Piece (x,y) : (firstCitizens (y:xs))
    else firstCitizens (y:xs)
firstCitizens (_:_) = []
firstCitizens [] = []

bstrPieces :: [Piece] -> BSL.ByteString
bstrPieces ps = BSL.concat $ map transformPiece ps
  where
    transformPiece (Piece (t1,t2)) =
        BSL.concat [transformTimeStamp t1, ",", transformTimeStamp t2, " \\\n"]
    transformTimeStamp (TimeStamp side str) =
        BSL.concat [BSL.pack $ show side, ":", str]

substituteInTemplate :: BSL.ByteString -> ScriptData -> BSL.ByteString
substituteInTemplate template (ScriptData (version, extension, source, pieces))
  = let subTable :: [(BS.ByteString, BSL.ByteString)]
        subTable = [ ( "{{VERSION}}",    BSL.concat ["\"", version,   "\""] )
                   , ( "{{EXTENSION}}",  BSL.concat ["\"", extension, "\""] )
                   , ( "{{SOURCE}}",     BSL.concat ["\"", source,    "\""] )
                   , ( "{{PIECES}}\n",   bstrPieces pieces )
                   ]
    in foldr (\(what, with) -> BSLS.replace what with) template subTable

-- this read is not safe (head/last may fail), but we support only valid scripts
readPiece :: BSL.ByteString -> Piece
readPiece strP = Piece ( readTimeStamp $ head splitted
                       , readTimeStamp $ last splitted )
  where
    splitted = BSL.split ',' strP

-- this read is not safe (read may fail), but we support only valid scripts
readTimeStamp :: BSL.ByteString -> TimeStamp
readTimeStamp strTS = let splitted = BSL.split ':' strTS
                      in TimeStamp (read $ BSL.unpack $ head splitted)
                                   (last splitted)

readScriptData :: BSL.ByteString -> ScriptData
readScriptData inpFileContents
  = ScriptData ( parseVar "VERSION="
               , parseVar "OUT_EXT="
               , parseVar "SOURCE_NAME="
               , parsePieces "for piece in \\\n"
               )
  where
    -- variables are read only inside quotes (first quote Char is just dropped)
    parseVar precStr = (BSL.takeWhile (/= '\"')) . (BSL.drop 1)
      $ BSL.takeWhile (/= '\n') $ (afterPart precStr)
    -- pieces are read line by line until ';'
    parsePieces precStr
      = if (parseVar "VERSION=") == scriptVersion
        then map readPiece $ BSL.lines
          $ BSL.takeWhile (/= ';') $ afterPart precStr
        -- unsupported script version, don't even try to parse pieces
        else []
    afterPart precStr
      = snd $ BSLS.breakAfter ('\n' `BS.cons` precStr) inpFileContents

-- add TimeStamp (with composing and adding new Pieces)
add :: BSL.ByteString -> BSL.ByteString -> TimeStamp -> BSL.ByteString
add inpFileContents mediaFileName t =
    let scriptData = readScriptData inpFileContents
    in if inpFileContents /= BSL.empty
       && (\(ScriptData (version, _, _, _)) -> version) scriptData == BSL.empty
       then BSL.concat [ "# attempt to use script with parser of version: "
                       , scriptVersion , "\n"
                       , inpFileContents
                       ]
       else substituteInTemplate scriptTemplateFile
                                 ( ScriptData ( scriptVersion
                                              , outFileExtension
                                              , nameFromFilename mediaFileName
                                              , pieces scriptData
                                              )
                                 )
         where
            notSE = filter (\(TimeStamp side _) -> side `notElem` [S,E])
            inpTimeStampsFromPieces = notSE . piecesToTs
            pieces (ScriptData (_, _, _, inpPieces))
              = if t `notElem` (inpTimeStampsFromPieces inpPieces)
                then allPieces $ t : (inpTimeStampsFromPieces inpPieces)
                else inpPieces
