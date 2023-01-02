module Bind (Bind, new) where

import Data.IORef (IORef, newIORef)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr)
import Id (Id, size)
import Prelude (IO, Int, return, ($), (*))

capacity :: Int
capacity =
  100 * 1000

data Bind = Bind
  { left :: Ptr Id,
    right :: Ptr Id,
    size :: IORef Int
  }

new :: IO Bind
new =
  do
    left' <- mallocBytes (capacity * Id.size)
    right' <- mallocBytes (capacity * Id.size)
    size' <- newIORef 0
    return $ Bind {left = left', right = right', size = size'}
