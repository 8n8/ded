module Ded (format) where

import Ast (Ast)
import Buf (Buf)
import Prelude (Either (Right), IO, String, return, ($))

format :: Buf -> Ast -> IO (Either String ())
format _ _ =
  return $ Right ()
