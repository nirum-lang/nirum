{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
-- How to add a test suite for a new target
--
-- a. Define a function of the same name to the target name
--    (i.e. `targetName :: Target t => Proxy t -> TargetName`),
--    which is a type of `IO ()`.  See `python :: IO ()` for example.
--
-- b. If a test suite requires any external programs list them
--    in "External dependencies" section of CONTRIBUTING.md docs.
--
-- c. Add the defined function to `do` block of `main` function.
--    Please keep functions in lexicographical order.
--
-- d. If an action is necessary for only CI builds invoke it using
--    `whenCi :: IO () -> IO ()`.
--
-- e. This script is written in Turtle, a DSL embeded in Haskell for
--    shell scripting.
--    See also <https://github.com/Gabriel439/Haskell-Turtle-Library>.
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
