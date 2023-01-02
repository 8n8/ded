module Order (Order, new) where

import Data.IORef (IORef, newIORef)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr)
import Id (Id, size)
import Prelude (IO, Int, return, ($), (*))

capacity :: Int
capacity =
  1000 * 1000

data Order = Order
  { order :: Ptr Id,
    size :: IORef Int
  }

new :: IO Order
new =
  do
    order' <- mallocBytes (Id.size * capacity)
    size' <- newIORef 0
    return $ Order {order = order', size = size'}
