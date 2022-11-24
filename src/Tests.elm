module Css exposing ( Style )


-}

import Css.Animations exposing (Keyframes)
import Css.Internal exposing (getOverloadedProperty, lengthConverter, lengthForOverloadedProperty)
import Css.Preprocess as Preprocess exposing (Style, unwrapSnippet)
import Css.String
import Css.Structure as Structure exposing (..)
import Hex
import String


 -}
type alias Style =
    Preprocess.Style



 -}


cssFunction : String -> List String -> String
cssFunction funcName args =
    funcName
        ++ "