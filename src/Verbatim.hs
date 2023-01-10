module Verbatim (Verbatim, new) where

import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)
import Data.IORef (IORef, newIORef)
import Foreign.Marshal.Alloc (mallocBytes)

data Verbatim
  = Verbatim (Ptr Word8) (Ptr Word32) (IORef Int)


meanLength :: Int
meanLength =
    20


maxItems :: Int
maxItems =
    100*1000


new :: IO Verbatim
new =
    do
    chars <- mallocBytes (meanLength * maxItems)
    ends <- mallocBytes (4 * maxItems)
    num <- newIORef 0
    return $ Verbatim chars ends num
