{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Control.Monad (when)
import System.Environment (lookupEnv)
import System.IO.Error (catchIOError, isDoesNotExistError)

import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (qq)

import Turtle

proc' :: T.Text -> [T.Text] -> IO ()
proc' prog args =
    catchIOError (procs prog args empty) $ \ex ->
        if isDoesNotExistError ex
        then do err ""
                err [qq|Couldn't find executable: "$prog"|]
                exit $ ExitFailure 1
        else ioError ex

main :: IO ()
main = do
    ciEnv <- lookupEnv "CI"
    let ci = maybe False (/= "") ciEnv

    -- Python target
    toxEnv <- lookupEnv "TOX"
    let tox = maybe "tox" T.pack toxEnv
    when ci $
        proc' tox ["-e", "devruntime"]
    proc' tox ["--skip-missing-interpreters"]
