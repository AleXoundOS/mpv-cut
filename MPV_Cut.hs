{-# LANGUAGE CPP #-} -- only for GHCi to avoid mess with foreign export objects
{-# LANGUAGE TemplateHaskell #-} -- only for embedFile
{-# LANGUAGE OverloadedStrings #-}
module MPV_Cut where
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import GHC.IO.Handle
import System.Posix.IO (fdToHandle, dup)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString (ByteString, useAsCString) --embedFile and atof
import Data.List ((\\), find, sort)

import Data.FileEmbed (embedFile)

import Debug.Trace
myTrace :: Show b => b -> b
myTrace x = trace ("\ntrace: " ++ show x) x

-- which side of piece: start | end | act as both | file start | file end
data Side = A | B | X | S | E
    deriving (Eq, Ord, Show)

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

version :: BSL.ByteString
version = "0.1" -- bash script format version

scriptTemplateFile :: Data.ByteString.ByteString
scriptTemplateFile = $(embedFile "script_template.sh")

foreign import ccall "stdio.h fileno" fileno :: Ptr CFile -> IO CInt
foreign import ccall unsafe "stdlib.h atof" c_atof :: CString -> IO CDouble
unsafeReadDouble :: BSL.ByteString -> CDouble
unsafeReadDouble = let toCString = Data.ByteString.useAsCString . BSL.toStrict
                   in unsafePerformIO . flip toCString c_atof

fpToHandle :: Ptr CFile -> IO Handle
fpToHandle fp = do
    int <- fileno fp
    -- get a duplicate of original file descriptor
    -- with combination of hClose avoids problem with locked files after write
    fd <- dup (fromIntegral int)
    handle <- fdToHandle fd
    return handle

#ifndef GHCI
foreign export ccall h_add :: Ptr CFile -> IO CInt
#endif
h_add :: Ptr CFile -> IO CInt
h_add fp = do
    h <- fpToHandle fp

    -- reading using duplicate Handle to avoid semi-closed Handle afterwards
    hSeek h AbsoluteSeek 0
    h2 <- hDuplicate h
    originalFileContents <- BSL.hGetContents h2

    -- processing and writing
    --hSeek h SeekFromEnd 0
    BSL.hPutStr h (add originalFileContents (TimeStamp A "0.0"))

    hClose h2
    hClose h
    return 0

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

adoptees :: [TimeStamp] -> [TimeStamp] -> [(TimeStamp,TimeStamp)]
adoptees remaining ts = foldr f [] remaining
  where
    f tA@(TimeStamp A s) acc = case (closest B (unsafeReadDouble s) ts) of
        Just tB -> (tA, tB) : acc
        Nothing -> acc
    f tB@(TimeStamp B s) acc = case (closest A (unsafeReadDouble s) ts) of
        Just tA -> (tA, tB) : acc
        Nothing -> acc
    f _ _ = error "unexpected side in adoptees"

borderTheSide :: TimeStamp -> (TimeStamp,TimeStamp)
borderTheSide t | getTimeStampSide t == A = (t, TimeStamp E "")
                | getTimeStampSide t == B = (TimeStamp S "", t)
                | otherwise = error "unexpected side in borderTheSide"

-- straight A-B from bottom to top level ++ closest A/B ++ bordered
allPieces :: [TimeStamp] -> [(TimeStamp,TimeStamp)]
allPieces ts =
    let lNatives = nativeCitizens ts
        lAdoptees = adoptees (ts \\ tuplesToList lNatives) ts
        -- remaining = foldl (\\) ts (map tuplesToList [lNatives, lAdoptees])
        remaining = foldr ((flip (\\)) . tuplesToList) ts [lNatives, lAdoptees]
        lBordered = map borderTheSide remaining
    in sort $ lNatives ++ lAdoptees ++ lBordered

tuplesToList :: [(a,a)] -> [a]
tuplesToList ((a,b):xs) = a : b : tuplesToList xs
tuplesToList _          = []

-- all straight A-B from bottom to top
nativeCitizens :: [TimeStamp] -> [(TimeStamp,TimeStamp)]
nativeCitizens ts =
    let pieces = firstCitizens ts
    in if not . null $ pieces
       then nativeCitizens (ts \\ tuplesToList pieces) ++ pieces
       else []
    -- maybe concatMap usage here

-- gets first straight A-B pieces, picking them out of the whole list
firstCitizens :: [TimeStamp] -> [(TimeStamp,TimeStamp)]
firstCitizens (x:y:xs) =
    if (getTimeStampSide x) `elem` [A,X] && (getTimeStampSide y) `elem` [B,X]
    then (x,y) : (firstCitizens (y:xs))
    else firstCitizens (y:xs)
firstCitizens (_:_) = []
firstCitizens [] = []

-- add TimeStamp to existing file
add :: BSL.ByteString -> TimeStamp -> BSL.ByteString
add originalFileContents t = BSL.append (myTrace originalFileContents) "Haskell here\n"
-- add originalFileContents t = BSL.fromStrict scriptTemplateFile
-- create new script if originalFileContents is empty
