module Ded (format) where

import qualified Data.ByteString

format ::
  Data.ByteString.ByteString ->
  Either String Data.ByteString.ByteString
format input =
  Right input
