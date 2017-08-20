{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Control.Monad (when)
import System.Environment (lookupEnv)
import System.IO.Error (catchIOError, isDoesNotExistError)

import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (qq)

import Turtle

-- | Invoke a process.
proc' :: T.Text -> [T.Text] -> IO ()
proc' prog args =
    catchIOError (procs prog args empty) $ \ex ->
        if isDoesNotExistError ex
        then do err ""
                err [qq|Couldn't find executable: "$prog"|]
                exit $ ExitFailure 1
        else ioError ex

-- | Run the IO operation only if it's run on CI.
whenCi :: IO () -> IO ()
whenCi block = do
    ciEnv <- lookupEnv "CI"
    let ci = maybe False (/= "") ciEnv
    when ci block

-- | Teget all targets.
main :: IO ()
main =
    python

-- | Test Python target.
python :: IO ()
python = do
    toxEnv <- lookupEnv "TOX"
    let tox = maybe "tox" T.pack toxEnv
    whenCi $ proc' tox ["-e", "devruntime"]
    proc' tox ["--skip-missing-interpreters"]
