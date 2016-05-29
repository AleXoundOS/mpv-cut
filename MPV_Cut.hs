{-# LANGUAGE CPP #-} -- only for GHCi to avoid mess with foreign export objects
{-# LANGUAGE TemplateHaskell #-} -- only for embedFile
{-# LANGUAGE OverloadedStrings #-} -- ByteStrings
-- |
-- Module      : MPV_Cut
-- Copyright   : (c) 2016 Alexander Tomokhov
--
-- License     : GPL3
-- Maintainer  : alexoundos@google.com
-- Stability   : experimental
-- Portability : GHC

module MPV_Cut where
import Foreign (poke)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import GHC.IO.Handle
import System.Posix.IO (fdToHandle, dup)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
  (ByteString, useAsCString, packCString, cons, hGetContents)
import Data.List ((\\), find, sort, nub, delete, isInfixOf)
import Text.Read (readMaybe)
import Data.FileEmbed (embedFile)
import Data.ByteString.Lazy.Search as BSLS (replace, breakAfter)
import System.Directory (doesFileExist, getDirectoryContents)

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

scriptTemplate :: BSL.ByteString
scriptTemplate = BSL.fromStrict $(embedFile "script_template.sh")

-- this is a Piece of stream, i.e. A-B, A-X, A-E, S-B, S-X, S-E
newtype Piece = Piece (TimeStamp,TimeStamp)
  deriving (Eq, Ord, Show)

newtype ScriptData
               --   version
  = ScriptData ( BSL.ByteString
               -- input filename   input name   output extension   audioOnly
               , BSL.ByteString, BSL.ByteString, BSL.ByteString, BSL.ByteString
               , [Piece] )
  deriving (Eq, Ord, Show)

data Direction = Forward | Backward
  deriving (Eq, Show)

data Position = First | Only | Last | Normal
  deriving (Show)

data RPosition = RSource | RFirst | ROnly | RLast | RNormal | RMissing
  deriving (Eq, Show)

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
foreign export ccall h_cfg :: Ptr CFile -> CString -> CString -> Char -> IO CInt
#endif
-- write configuration (version, filename, name, extension)
h_cfg :: Ptr CFile -> CString -> CString -> Char -> IO CInt
h_cfg fp cMediaFilename cExtension cAudioOnly = do
    h <- fpToHandle fp

    mediaFilenameBind <- BS.packCString cMediaFilename
    let mediaFilename = BSL.fromStrict mediaFilenameBind

    extensionBind <- BS.packCString cExtension
    let realExtension = let extension = BSL.fromStrict extensionBind
                        in if extension /= BSL.empty
                           -- use given extension
                           then extension
                           -- extract it from input filename, keep original
                           else extFromFilename mediaFilename

    let audioOnly = if cAudioOnly == '\0'
                    then "False"
                    else "True"

    -- writing all variables in script but with empty Pieces
    BSL.hPutStr h
      $ substituteInTemplate (ScriptData ( scriptVersion
                                         , mediaFilename
                                         , nameFromFilename mediaFilename
                                         , realExtension
                                         , audioOnly
                                         , [] ))
    hClose h
    return 0

#ifndef GHCI
foreign export ccall h_add :: Ptr CFile -> Char -> CString -> IO CInt
#endif
-- add timestamp
h_add :: Ptr CFile -> Char -> CString -> IO CInt
h_add fp c_side c_time = do
    let maybeSide :: Maybe Side
        maybeSide = readMaybe [c_side]
    if maybeSide == Nothing
        then do
            return 2
        else do
            -- | fetching parameters for add
            let side = (\(Just x) -> x) maybeSide

            timeBind <- BS.packCString c_time
            let time = BSL.fromStrict timeBind

            h <- fpToHandle fp

            -- read using duplicate Handle - avoid semi-closed Handle afterwards
            h2 <- hDuplicate h
            hSeek h2 AbsoluteSeek 0
            inpFileContentsStrict <- BS.hGetContents h2
            let inpFileContents = BSL.fromStrict inpFileContentsStrict

            let returnWithClose code = do hClose h2
                                          hClose h
                                          return code

            -- processing and writing
            hSeek h AbsoluteSeek 0
            case add inpFileContents (TimeStamp side time) of
                Left bstr -> do BSL.hPutStr h bstr
                                returnWithClose 0
                Right bstrError -> if bstrError == "already added"
                                   then do returnWithClose 3
                                   else do BSL.hPutStr h
                                             $ BSL.append bstrError
                                                          inpFileContents
                                           returnWithClose 1

#ifndef GHCI
foreign export ccall h_nav
  :: Ptr CFile -> CString -> Char -> Ptr Char -> Ptr Char -> Ptr CString
  -> IO CInt
#endif
h_nav :: Ptr CFile -> CString -> Char -> Ptr Char -> Ptr Char -> Ptr CString
      -> IO CInt
h_nav fp c_inTime c_d pPos pSide pTime = do
    h <- fpToHandle fp

    -- file read
    h2 <- hDuplicate h
    hSeek h2 AbsoluteSeek 0
    inpFileContentsStrict <- BS.hGetContents h2
    let inpFileContents = BSL.fromStrict inpFileContentsStrict

    -- Time
    inTimeBind <- BS.packCString c_inTime
    let inTime = BSL.fromStrict inTimeBind

    let returnWithClose code = do hClose h2
                                  hClose h
                                  return code

    if c_d `notElem` ['f', 'b']
        then do
            returnWithClose 2
        else do
            -- Direction
            let d = case c_d of
                        'f' -> Forward
                        'b' -> Backward
                        _   -> error "unexpected direction"

            hSeek h AbsoluteSeek 0
            case nav inpFileContents inTime d of
                Left (Just (TimeStamp side time, position)) -> do
                    -- write position by pointer
                    poke pPos $ myToLower $ ((show position) !! 0)
                    -- write side by pointer
                    poke pSide $ (show side) !! 0
                    -- write time pointer
                    timeCStr <- BS.useAsCString (BSL.toStrict time) return
                    poke pTime timeCStr

                    returnWithClose 0

                Left Nothing -> do
                    -- write position by pointer as '\0'
                    poke pPos '\0'
                    -- write side pointer as '\0'
                    poke pSide '\0'
                    -- write time pointer as nullPtr
                    poke pTime nullPtr

                    returnWithClose 0

                Right bstr -> do BSL.hPutStr h (BSL.append bstr inpFileContents)
                                 returnWithClose 1

#ifndef GHCI
foreign export ccall h_del :: Ptr CFile -> CString -> IO CInt
#endif
h_del :: Ptr CFile -> CString -> IO CInt
h_del fp c_time = do
    h <- fpToHandle fp

    -- read using duplicate Handle - avoid semi-closed Handle afterwards
    h2 <- hDuplicate h
    hSeek h2 AbsoluteSeek 0
    inpFileContentsStrict <- BS.hGetContents h2
    let inpFileContents = BSL.fromStrict inpFileContentsStrict

    timeBind <- BS.packCString c_time
    let time = BSL.fromStrict timeBind

    let returnWithClose code = do hClose h2
                                  hClose h
                                  return code

    -- process and write
    hSeek h AbsoluteSeek 0
    case del inpFileContents time of
        Left bstr -> do BSL.hPutStr h bstr
                        -- truncate file (so that there is no garbage at end)
                        hSetFileSize h $ fromIntegral (BSL.length bstr)
                        returnWithClose 0
        Right bstrError -> if bstrError == "does not exist"
                           then do returnWithClose 3
                           else do BSL.hPutStr h
                                     $ BSL.append bstrError inpFileContents
                                   returnWithClose 1

#ifndef GHCI
foreign export ccall h_res :: Ptr CFile -> CString -> CString -> Char
                           -> Ptr CString
                           -> IO CInt
#endif
h_res :: Ptr CFile -> CString -> CString -> Char -> Ptr CString -> IO CInt
h_res fp cWDir cSrcFilename c_d pResFilename = do
    h <- fpToHandle fp

    -- file read
    h2 <- hDuplicate h
    hSeek h2 AbsoluteSeek 0
    inpFileContentsStrict <- BS.hGetContents h2
    let inpFileContents = BSL.fromStrict inpFileContentsStrict
    wDir <- BS.packCString cWDir
    srcFilename <- BS.packCString cSrcFilename
    let direction = case c_d of
                        'p' -> Backward
                        'n' -> Forward
                        _   -> error "unexpected direction in h_res"

    result <- res inpFileContents (BSL.fromStrict wDir)
                                  (BSL.fromStrict srcFilename)
                                  direction
    let position (p, _) = p

    resFilenameCStr <- BS.useAsCString (BSL.toStrict $ (\(_, f) -> f) result)
                                       return
    poke pResFilename resFilenameCStr

    hClose h2
    hClose h

    return $ fromIntegral $ fromEnum $ myToLower $ (show $ position result) !! 1

myToLower :: Char -> Char
myToLower c = toEnum (fromEnum c + 32)

nameFromFilename :: BSL.ByteString -> BSL.ByteString
nameFromFilename filename
  = BSL.reverse $ BSL.drop 1 $ BSL.dropWhile (/= '.') $ (BSL.reverse filename)

extFromFilename :: BSL.ByteString -> BSL.ByteString
extFromFilename filename
  = check $ BSL.reverse $ BSL.takeWhile (/= '.') (BSL.reverse filename)
  where
    -- check whether resulting string is not equal to original
    check string = if string /= filename
                   then string
                   else BSL.empty

-- get closest A/B TimeStamp from the supplied list against specific time
-- A is searched backward, B is searched forward
closest :: Side -> CDouble -> [TimeStamp] -> Maybe TimeStamp
closest s d ts
    | s == A = find ((== A) . getTimeStampSide) tsBackward
    | s == B = find ((== B) . getTimeStampSide) tsForward
    | otherwise = error "unexpected side in closest"
  where
    st = sort ts
    tsForward =  dropWhile (( <= d ) . getTimeStampDouble) st
    tsBackward = reverse $ takeWhile ((  < d ) . getTimeStampDouble) st

adoptees :: [TimeStamp] -> [TimeStamp] -> [Piece]
adoptees remaining ts = foldr f [] remaining
  where
    f tA@(TimeStamp A st) acc = fm tA (closest B (unsafeReadDouble st) ts) acc
    f tB@(TimeStamp B st) acc = fm tB (closest A (unsafeReadDouble st) ts) acc
    f tX@(TimeStamp X st) acc
      = foldr (fm tX) acc [ closest B (unsafeReadDouble st) ts
                          , closest A (unsafeReadDouble st) ts
                          ]
    f _ _ = error "unexpected side in adoptees"
    -- from maybe x y to (Piece (x/y, y/x) : acc) or acc
    fm x (Just y@(TimeStamp B _)) acc = Piece (x, y) : acc
    fm x (Just y@(TimeStamp A _)) acc = Piece (y, x) : acc
    fm _ Nothing acc = acc
    fm _ (Just _) _ = error "unexpected side in adoptees 2"

-- compose S-B, A-E
borderTimeStamps :: [TimeStamp] -> [Piece]
borderTimeStamps (t@(TimeStamp A _):ts)
  = Piece (t, TimeStamp E "null") : borderTimeStamps ts
borderTimeStamps (t@(TimeStamp B _):ts)
  = Piece (TimeStamp S "null", t) : borderTimeStamps ts
borderTimeStamps (TimeStamp _ _:_) = error "unexpected side in borderTimeStamp"
borderTimeStamps [] = []

-- compose S-X and X-E Pieces if X is first or last
borderEdgeTs :: [TimeStamp] -> [Piece]
borderEdgeTs [] = []
borderEdgeTs ts = checkAndBorderE (last ts) $ checkAndBorderS (head ts) []
  where
    checkAndBorderS :: TimeStamp -> [Piece] -> [Piece]
    checkAndBorderS t@(TimeStamp X _) = (:) (Piece (TimeStamp S "null", t))
    checkAndBorderS _ = id
    checkAndBorderE :: TimeStamp -> [Piece] -> [Piece]
    checkAndBorderE t@(TimeStamp X _) = (:) (Piece (t, TimeStamp E "null"))
    checkAndBorderE _ = id

-- straight A-B from bottom to top level ++ closest A/B ++ bordered
allPieces :: [TimeStamp] -> [Piece]
allPieces ts
  = let sts = sort ts
        lNatives = nativeCitizens sts
        lAdoptees = adoptees (sts \\ piecesToTs lNatives) sts
        lBorderedX = borderEdgeTs sts
        remaining = foldl (\\) ts (map piecesToTs [ lNatives
                                                  , lAdoptees
                                                  , lBorderedX ])
        -- remaining = foldr ((flip (\\)) . piecesToTs) sts [ lNatives
                                                         -- , lAdoptees
                                                         -- , lBorderedX ]
        lBordered = borderTimeStamps remaining
    in sort $ lNatives ++ lAdoptees ++ lBordered ++ lBorderedX

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

-- this read is not safe (head/last may fail), but we support only valid scripts
readPiece :: BSL.ByteString -> Piece
readPiece strP = Piece ( readTimeStamp $ head splitted
                       , readTimeStamp $ last splitted )
  where
    splitted = BSL.split ',' (BSL.takeWhile (/= ' ') strP)

-- this read is not safe (read may fail), but we support only valid scripts
readTimeStamp :: BSL.ByteString -> TimeStamp
readTimeStamp strTS = let splitted = BSL.split ':' strTS
                      in TimeStamp (read $ BSL.unpack $ head splitted)
                                   (last splitted)

readScriptData :: BSL.ByteString -> ScriptData
readScriptData inpFileContents
  = ScriptData ( parseVar "VERSION="
               , parseVar "IN_FILENAME="
               , parseVar "IN_NAME="
               , parseVar "OUT_EXT="
               , parseVar "AUDIO_ONLY="
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

substituteInTemplate :: ScriptData -> BSL.ByteString
substituteInTemplate (ScriptData ( version
                                 , filename, name, extension, audioOnly
                                 , pieces ))
  = let subTable :: [(BS.ByteString, BSL.ByteString)]
        subTable = [ ( "{{VERSION}}",     BSL.concat ["\"", version,   "\""] )
                   , ( "{{IN_FILENAME}}", BSL.concat ["\"", filename,  "\""] )
                   , ( "{{IN_NAME}}",     BSL.concat ["\"", name,      "\""] )
                   , ( "{{EXTENSION}}",   BSL.concat ["\"", extension, "\""] )
                   , ( "{{AUDIO_ONLY}}",  BSL.concat ["\"", audioOnly, "\""] )
                   , ( "{{PIECES}}\n",    bstrPieces pieces )
                   ]
    in foldr (\(what, with) -> BSLS.replace what with) scriptTemplate subTable

checkScript :: ScriptData -> BSL.ByteString
checkScript (ScriptData (version, filename, name, extension, audioOnly, _))
  | version /= scriptVersion
    = BSL.concat [ "# attempt to use script with parser of version: "
                 , scriptVersion , "\n"
                 ]
  | filename  == BSL.empty = "# couldn't read IN_FILENAME\n"
  | name      == BSL.empty = "# couldn't read IN_NAME\n"
  | extension == BSL.empty = "# couldn't read OUT_EXT\n"
  | audioOnly == BSL.empty = "# couldn't read AUDIO_ONLY\n"
  | otherwise = BSL.empty

inpTimeStamps :: [Piece] -> [TimeStamp]
inpTimeStamps ps
  = let notSE = filter (\(TimeStamp side _) -> side `elem` [A,B,X])
        inpTimeStampsFromPieces = notSE . piecesToTs
    in nub (inpTimeStampsFromPieces ps)

closestAny :: [TimeStamp] -> CDouble -> Direction -> Maybe TimeStamp
closestAny ts time d = case d of
    Forward  -> if (not . null) tsForward
                then Just $ head tsForward
                else Nothing
    Backward -> if (not . null) tsBackward
                then Just $ head tsBackward
                else Nothing
  where
    st = sort ts
    tsForward =  dropWhile (( <= time ) . getTimeStampDouble) st
    tsBackward = reverse $ takeWhile ((  < time ) . getTimeStampDouble) st

-- add TimeStamp (with composing and adding new Pieces)
add :: BSL.ByteString -> TimeStamp -> Either BSL.ByteString BSL.ByteString
add inpFileContents t =
    let scriptData = readScriptData inpFileContents
        inpPieces = (\(ScriptData (_, _, _, _, _, x)) -> x) scriptData
        ts = inpTimeStamps inpPieces
    in if inpFileContents /= BSL.empty && checkScript scriptData /= BSL.empty
       then Right $ checkScript scriptData
       else if t `elem` ts
            then Right "already added"
            else let newScriptData (ScriptData ( _, filename, name, extension
                                               , audioOnly, _ ))
                       = substituteInTemplate ( ScriptData ( scriptVersion
                                                           , filename
                                                           , name
                                                           , extension
                                                           , audioOnly
                                                           , allPieces $ t : ts
                                                           )
                                              )
                 in Left $ newScriptData scriptData

-- navigate in existing script forward or backward from given time
nav :: BSL.ByteString -> BSL.ByteString -> Direction
    -> Either (Maybe (TimeStamp, Position)) BSL.ByteString
nav inpFileContents time d
  = let scriptData = readScriptData inpFileContents
        inpPieces = (\(ScriptData (_, _, _, _, _, x)) -> x) scriptData
        ts = sort $ inpTimeStamps inpPieces
        timeDouble = unsafeReadDouble time
    -- check if there are parsing errors of existing script (if supplied)
    in if inpFileContents /= BSL.empty && checkScript scriptData /= BSL.empty
       -- return parsing error message string
       then Right $ checkScript scriptData
       -- proceed next as script is parsed successfully
       else Left
         -- first find closest TimeStamp except identical bordered to supplied
         -- if no closest, find among all existing TimeStamps including bordered
         $ let maybeT = case closestAny ts timeDouble d of
                            Just t -> Just t
                            Nothing -> find (\t -> getTimeStampStr t == time) ts
           in case maybeT of
                          -- determine position of existing TimeStamp
                Just t -> let p | length ts == 1 = Only
                                | t == (head ts) = First
                                | t == (last ts) = Last
                                | otherwise      = Normal
                          in Just (t, p)
                Nothing -> Nothing

-- delete timestamp at specified time
del :: BSL.ByteString -> BSL.ByteString -> Either BSL.ByteString BSL.ByteString
del inpFileContents time
  = let scriptData = readScriptData inpFileContents
        inpPieces = (\(ScriptData (_, _, _, _, _, x)) -> x) scriptData
        ts = inpTimeStamps inpPieces
        t = TimeStamp X time -- here we assume that equality ignores Side
        newScriptData (ScriptData (_, filename, name, extension, audioOnly, _))
          = ScriptData ( scriptVersion, filename, name, extension, audioOnly
                       , allPieces $ delete t ts )
    in if inpFileContents /= BSL.empty && checkScript scriptData /= BSL.empty
       then Right $ checkScript scriptData
       else if t `notElem` ts
            then Right "does not exist"
            else Left $ substituteInTemplate (newScriptData scriptData)

show2d :: Int -> BSL.ByteString
show2d n | length (show n) == 1 = '0' `BSL.cons` (BSL.pack (show n))
         | otherwise = BSL.pack $ show n

findFileByPatternInDir :: BSL.ByteString -> BSL.ByteString -> IO BSL.ByteString
findFileByPatternInDir pattern dir = do
    dirList <- getDirectoryContents (BSL.unpack dir)
    let nonNumberCondition :: String -> Bool
        nonNumberCondition with
          = notElem (BSL.unpack
                      $ (extFromFilename . nameFromFilename) (BSL.pack with))
                    [BSL.unpack $ show2d x | x <- [00..99]]
        predicate :: String -> Bool
        predicate with = isInfixOf (BSL.unpack pattern) with
                      && nonNumberCondition with
                      && extFromFilename (BSL.pack with) /= "sh"
        satisfied :: [FilePath]
        satisfied = filter predicate dirList
    if (not . null) satisfied
    then return $ BSL.pack $ head satisfied
    else return BSL.empty

res :: BSL.ByteString -> BSL.ByteString -> BSL.ByteString -> Direction
    -> IO (RPosition, BSL.ByteString)
res inpFileContents wDir srcFilename direction
  = let ext = (\(ScriptData (_, _, _, e, _, _)) -> e)
                $ readScriptData inpFileContents
        srcName = nameFromFilename srcFilename
        srcMaybeNum = case (BSL.readInt . extFromFilename) srcName of
                        Just (n, _) -> Just n
                        Nothing -> Nothing
        rNum :: Maybe Int -> Direction -> Int
        rNum (Just number) Forward  = number + 1
        rNum Nothing       Forward  = 0
        rNum (Just number) Backward = number - 1
        rNum Nothing       Backward = -1 -- source file
        reqFilename :: Int -> IO BSL.ByteString
        reqFilename reqNum
          = if reqNum == -1
            then if srcMaybeNum == Nothing
                 then return srcFilename
                 else do
                     maybeFound <- findFileByPatternInDir
                                   (nameFromFilename srcName) wDir
                     return maybeFound
            else return $ BSL.concat [ if srcMaybeNum == Nothing
                                       then srcName
                                       else nameFromFilename srcName
                                     , BSL.cons '.' (show2d reqNum)
                                     , BSL.cons '.' ext ]
        position :: Int -> IO RPosition
        position (-1) = return RSource
        position 00 = do
            -- check if more then single 00 exists
            filename <- reqFilename 00
            fileExists <- doesFileExist $ BSL.unpack filename
            if fileExists
            then do
                -- check if more then single 00 exists
                filename01 <- reqFilename 01
                fileExists01 <- doesFileExist $ BSL.unpack filename01
                if fileExists01
                then return RFirst -- 01 also exists
                else return ROnly -- 00 only
            else return RMissing -- no cutted video pieces exist
        position number = do
            -- check if it exists
            filename <- reqFilename number
            fileExists <- doesFileExist $ BSL.unpack filename
            if fileExists
            then do
                -- check if it's last
                filename_last <- reqFilename (number + 1)
                fileExists_last <- doesFileExist $ BSL.unpack filename_last
                if fileExists_last
                then return RNormal
                else return RLast
            else if number == 01
                 then return ROnly
                 else return RNormal
    in do
        if ext == BSL.empty
        then error "couldn't parse extension from script"
        else do filename <- reqFilename $ rNum srcMaybeNum direction
                fileExists <- doesFileExist $ BSL.unpack filename
                p <- position $ myTrace $ rNum srcMaybeNum direction
                return (myTrace p, if fileExists
                           then filename
                           else if p /= RMissing
                                then srcFilename
                                else BSL.empty)
