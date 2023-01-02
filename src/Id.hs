module Id (Id, Id.new, verbatim, size) where

import Data.Bits (shiftL, (.|.))
import Data.Word (Word32, Word8)
import Row (Row, toInt)
import Prelude (Int, fromIntegral)

size :: Int
size =
  4

newtype Id
  = Id Word32

newtype Table
  = Table Word8

verbatim :: Table
verbatim =
  Table 0

new :: Table -> Row -> Id
new (Table table) row =
  Id (((fromIntegral table) `shiftL` 24) .|. Row.toInt row)
