module Buf (Buf, new, fill) where


import Data.IORef (IORef, newIORef, writeIORef)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (mallocBytes)


data Buf
    = Buf (Ptr Word8) (IORef Int)


write :: Buf -> (Ptr Word8 -> Int -> IO Int) -> IO ()
write (Buf buf sizeRef) filler =
    do
    n <- filler buf maxBuf
    writeIORef sizeRef n


read :: Buf -> (Ptr Word8 -> Int -> IO ()) -> IO ()
read 


maxBuf :: Int
maxBuf =
    10 * 1000 * 1000


new :: IO Buf
new =
    do
    buf <- mallocBytes maxBuf
    size <- newIORef 0
    return $ Buf buf size
