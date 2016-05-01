{-# LANGUAGE ScopedTypeVariables #-}

module MPV_Cut where

import Foreign.Ptr
import Foreign.C.Types
import GHC.IO.Handle
import System.Posix.IO (fdToHandle)

foreign import ccall "stdio.h fileno" fileno :: Ptr CFile -> IO CInt

foreign export ccall
    add_c :: Ptr CFile -> IO CInt
add_c :: Ptr CFile -> IO CInt
add_c h = do
    fd <- fileno h
    handle <- fdToHandle $ fromIntegral fd
    hSetBuffering handle LineBuffering
    add handle
    --hFlush handle
    return 0

add :: Handle -> IO ()
add h = hPutStr h "Haskell is here\n"
