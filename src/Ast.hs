module Ast (Ast, Ast.new) where

import Bind (Bind, new)
import Data.IORef (IORef, newIORef)
import Id (Id)
import Order (Order, new)
import Verbatim (Verbatim, new)
import Prelude (IO, Maybe (Nothing), return, ($))

data Ast = Ast
  { verbatim :: Verbatim,
    moduleName :: IORef (Maybe Id),
    exposing :: IORef (Maybe Id),
    bind :: Bind,
    order :: Order
  }

new :: IO Ast
new =
  do
    verbatim' <- Verbatim.new
    moduleName' <- newIORef Nothing
    exposing' <- newIORef Nothing
    bind' <- Bind.new
    order' <- Order.new
    return $
      Ast
        { verbatim = verbatim',
          moduleName = moduleName',
          exposing = exposing',
          bind = bind',
          order = order'
        }
