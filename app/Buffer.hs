module Buffer (Buffer, new, fill) where

import qualified Control.Exception
import qualified Data.Word
import qualified Foreign.Marshal.Alloc
import qualified Foreign.Ptr
import qualified Foreign.Storable
import qualified System.IO
import qualified System.IO.Error

data Buffer
  = Buffer
      (Foreign.Ptr.Ptr Data.Word.Word32) -- how much of the buffer is used
      (Foreign.Ptr.Ptr Data.Word.Word8) -- the buffer

-- | It seems unlikely that an Elm code file would be bigger than 10MB.
maxSize :: Int
maxSize =
  10 * 1000 * 1000

new :: IO Buffer
new =
  do
    size <- Foreign.Marshal.Alloc.malloc
    buffer <- Foreign.Marshal.Alloc.mallocBytes maxSize
    return (Buffer size buffer)

data FileError
  = DoesNotExist
  | Permission
  | General String

fill :: Buffer -> System.IO.Handle -> IO (Either FileError ())
fill buffer handle =
  do
    result <- fillCaught buffer handle
    case result of
      Left err ->
        if System.IO.Error.isDoesNotExistError err
          then return (Left DoesNotExist)
          else
            if System.IO.Error.isPermissionError err
              then return (Left Permission)
              else return (Left (General (show err)))
      Right () ->
        return (Right ())

fillCaught :: Buffer -> System.IO.Handle -> IO (Either IOError ())
fillCaught buffer handle =
  Control.Exception.try (fillFromHandle buffer handle)

fillFromHandle :: Buffer -> System.IO.Handle -> IO ()
fillFromHandle (Buffer sizePointer bufferPointer) handle =
  do
    size <- Foreign.Storable.peek sizePointer
    let intSize = fromIntegral size :: Int
    numBytesRead <- System.IO.hGetBuf handle bufferPointer intSize
    let intNumBytes = fromIntegral numBytesRead :: Data.Word.Word32
    _ <- Foreign.Storable.poke sizePointer intNumBytes
    return ()
