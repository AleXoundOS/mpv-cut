{-# LANGUAGE CPP #-} -- only for GHCi to avoid mess with foreign export objects
{-# LANGUAGE TemplateHaskell #-} -- only for embedFile
module MPV_Cut where
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import GHC.IO.Handle
import System.Posix.IO (fdToHandle, dup)
import Data.FileEmbed (embedFile)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString (ByteString, useAsCString) --embedFile and atof
import Data.List ((\\))

-- which side of piece: start | end | act as both
data Side = A | B | X
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
    input <- BSL.hGetContents h2

    -- processing and writing
    --hSeek h SeekFromEnd 0
    BSL.hPutStr h (add input)

    hClose h2
    hClose h
    return 0

-- allPieces :: [TimeStamp] -> [(TimeStamp,TimeStamp)]
-- allPieces ts = firstClassPieces ts []

firstClassPieces :: [TimeStamp] -> [(TimeStamp,TimeStamp)]
firstClassPieces ts = let pieces = firstCitizens ts
    in if not . null $ pieces
       then firstClassPieces (ts \\ tuplesToList pieces) ++ pieces
       else [] -- once list is being exhausted

tuplesToList :: [(a,a)] -> [a]
tuplesToList ((a,b):xs) = a : b : tuplesToList xs
tuplesToList _          = []

-- gets first straight A-B pieces, picking them out of the whole list
firstCitizens :: [TimeStamp] -> [(TimeStamp,TimeStamp)]
firstCitizens (x:y:xs) =
    if (getTimeStampSide x) `elem` [A,X] && (getTimeStampSide y) `elem` [B,X]
    then (x,y) : (firstCitizens (y:xs))
    else firstCitizens (y:xs)
firstCitizens (_:_) = []
firstCitizens [] = []

add :: BSL.ByteString -> BSL.ByteString
add input = BSL.fromStrict scriptTemplateFile
