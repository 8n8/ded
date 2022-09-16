port module Main exposing (main)

import Platform
import Json.Decode
import Json.Encode


main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ((), Cmd Msg)
init _ =
    ((), Cmd.none)


update : Msg -> () -> ((), Cmd Msg)
update (FromJs _) () =
    ((), Cmd.none)


type Msg
    = FromJs Json.Decode.Value


port fromJs : (Json.Decode.Value -> msg) -> Sub msg


port toJs : Json.Encode.Value -> Cmd msg


subscriptions : () -> Sub Msg
subscriptions _ =
    fromJs FromJs
