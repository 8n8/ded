module Buf (Buf, get, append, new) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)

data Buf
  = Buf (Ptr Word8) (IORef Int)

capacity :: Int
capacity =
  10 * 1000 * 1000

new :: IO Buf
new =
  do
    ptr <- mallocBytes capacity
    size <- newIORef 0
    return $ Buf ptr size

append :: Buf -> Word8 -> IO (Maybe ())
append (Buf ptr sizeRef) value =
  do
    oldSize <- readIORef sizeRef
    if oldSize == capacity
      then return Nothing
      else do
        pokeElemOff ptr oldSize value
        writeIORef sizeRef (oldSize + 1)
        return $ Just ()

get :: Buf -> Int -> IO (Maybe Word8)
get (Buf ptr sizeRef) index =
  do
    size <- readIORef sizeRef
    if index >= size || index < 0
      then return Nothing
      else do
        byte <- peekElemOff ptr index
        return $ Just byte
