{-# LANGUAGE TemplateHaskell #-}
module MPV_Cut where
import Foreign.Ptr
import Foreign.C.Types
import GHC.IO.Handle
import System.Posix.IO (fdToHandle)
import qualified Data.ByteString as B
import Data.FileEmbed (embedFile)

scriptTemplateFile :: B.ByteString
scriptTemplateFile = $(embedFile "script_template.sh")

foreign import ccall "stdio.h fileno" fileno :: Ptr CFile -> IO CInt

foreign export ccall h_add :: Ptr CFile -> IO CInt
h_add :: Ptr CFile -> IO CInt
h_add fp = do
    fd <- fileno fp
    handle <- fdToHandle $ fromIntegral fd
    --hSetBuffering handle LineBuffering
    add handle
    hFlush handle
    return 0

add :: Handle -> IO ()
add h = do
    hSeek h AbsoluteSeek 0
    --line <- hGetLine h
    --hPutStr h line
    h2 <- hDuplicate h
    string <- hGetContents h2
    hPutStr h string
    B.hPutStr h scriptTemplateFile
