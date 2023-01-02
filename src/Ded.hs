module Ded (format) where

import Buf (Buf)

format :: Buf -> IO (Either String ())
format _ =
  return $ Right ()
