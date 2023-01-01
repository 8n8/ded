module Ded (format) where

import qualified Data.ByteString

format ::
  Data.ByteString.ByteString ->
  Either String Data.ByteString.ByteString
format _ =
  Right
  "module X exposing (x)\n\
  \\n\
  \\n\
  \x =\n\
  \    0\n\
  \"
