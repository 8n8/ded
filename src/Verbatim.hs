module Verbatim (Verbatim, new) where

import Data.IORef (IORef, newIORef)
import Data.Word (Word32, Word8)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr)
import Prelude (IO, Int, return, ($), (*))

capacity :: Int
capacity =
  100 * 1000

meanVerbatimSize :: Int
meanVerbatimSize =
  10

data Verbatim = Verbatim
  { verbatim :: Ptr Word8,
    ends :: Ptr Word32,
    size :: IORef Int
  }

new :: IO Verbatim
new =
  do
    verbatim' <- mallocBytes (meanVerbatimSize * capacity)
    ends' <- mallocBytes (capacity * 4)
    size' <- newIORef 0
    return $
      Verbatim
        { verbatim = verbatim',
          ends = ends',
          size = size'
        }
