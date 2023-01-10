module Ded (format) where

import Buf (Buf)
import Ast (Ast)


format :: Ast -> Buf -> IO (Either String ())
format _ _ =
    return $ Right ()
