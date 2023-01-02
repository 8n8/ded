module Row (Row, new, toInt) where

import Data.Word (Word32)
import Prelude (Maybe (Just, Nothing), ($), (*), (-), (>))

newtype Row
  = Row Word32

toInt :: Row -> Word32
toInt (Row row) =
  row

new :: Word32 -> Maybe Row
new raw =
  if raw > (256 * 256 * 256 - 1)
    then Nothing
    else Just $ Row raw
