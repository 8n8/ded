module Ast (Ast, Ast.new) where

import Verbatim (Verbatim, new)

data Ast
    = Ast
        Verbatim


new :: IO Ast
new =
    do
    verbatim <- Verbatim.new
    return $ Ast verbatim
