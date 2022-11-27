module Main (main) where

import qualified System.IO
import qualified Control.Exception
import qualified System.Info
import qualified System.Directory
import qualified Buffer

main :: IO ()
main =
    do
    buffer1 <- Buffer.new
    buffer2 <- Buffer.new
    run buffer1 buffer2 "."


type Buffer = Buffer.Buffer


run :: Buffer -> Buffer -> String -> IO ()
run buffer1 buffer2 path =
    do
    fileResult <- runOnFile path buffer1 buffer2
    case fileResult of
        Ok ->
            return ()

        Fail ->
            do
            dirResult <-
                Control.Exception.try (System.Directory.listDirectory path)
                    :: IO (Either IOError [String])
            case dirResult of
                Left _ ->
                    return ()

                Right dirs ->
                    do
                    let joined = map (joinPath path) dirs
                    mapM_ (run buffer1 buffer2) joined


joinPath :: String -> String -> String
joinPath before after =
    mconcat
    [ before
    , if System.Info.os == "mingw32" then
        "\\"
      else
        "/"
    , after
    ]
    
            


data RunResult
    = Ok
    | Fail


runOnFile :: String -> Buffer -> Buffer -> IO RunResult
runOnFile path buffer1 buffer2 =
    if isElmPath path then
        do
        Control.Exception.try $
        System.IO.withFile path System.IO.ReadMode (formatFile buffer1 buffer2)
            :: IO (Either IOError ())
        case result of
            Left () ->
                return Fail

            Right () ->
                do
                Buffer.writeToFile
                return Ok

    else
        return Fail    


isElmPath :: String -> Bool
isElmPath path =
    case reverse path of
        'm' : 'l' : 'e' : '.' : _ : _ ->
            True

        _ ->
            False


formatFile :: Buffer -> Buffer -> System.IO.Handle -> IO (Maybe Buffer)
formatFile buffer1 buffer2 handle =
    do
    result <- Buffer.fill buffer1 handle
    case err of
        Left () ->
            return Nothing

        Right () ->
            do
            formatExpression buffer1 buffer2

