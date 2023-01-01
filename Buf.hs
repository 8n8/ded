module Buf (Buf, toString, fromString) where

data Buf
  = Buf (Foreign.Ptr.Ptr Data.Word.Word8) (Data.IORef.IORef Int)


toString :: Buf -> IO String
toString (Buf textPtr sizeRef) =
  do
  size <- Data.IORef.readIORef sizeRef
  toStringHelp textPtr size (Before "")


toStringHelp :: Foreign.Ptr.Ptr Data.Word.Word8 -> Int -> Accum -> IO String
toStringHelp textPtr size accum =
  case accum of
    Before soFar index ->
      if index == size then
        return (reverse soFar)

      else
        do
        byte <- Foreign.Storable.peekByteOff textPtr (index + 1)
        if byte >> 7 == 0 then
          toStringHelp
            textPtr
            size
            (Before (Data.Char.chr byte : soFar) (index + 1))

        else if byte >> 5 == 6 then
          toStringHelp
            textPtr
            size
            (
