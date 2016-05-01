--{-# LANGUAGE ScopedTypeVariables #-}

module MPV_Cut where

import Foreign.Ptr
import Foreign.C.Types
import GHC.IO.Handle
import System.Posix.IO (fdToHandle)

foreign import ccall "stdio.h fileno" fileno :: Ptr CFile -> IO CInt

foreign export ccall h_add :: Ptr CFile -> IO CInt
h_add :: Ptr CFile -> IO CInt
h_add h = do
    fd <- fileno h
    handle <- fdToHandle $ fromIntegral fd
    hSetBuffering handle LineBuffering
    add handle
    --hFlush handle
    return 0

add :: Handle -> IO ()
add h = hPutStr h "Haskell is here\n"
