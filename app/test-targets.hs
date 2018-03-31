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
import Data.String (IsString (fromString))
import System.Environment (lookupEnv)
import System.IO.Error (catchIOError, isDoesNotExistError)

import System.IO.Temp
import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (qq)
import Turtle

-- | Invoke a process if the given 'program' exists.
-- Invoke a fallback function if it does not exist.
tryProc :: T.Text -> [T.Text] -> IO () -> IO ()
tryProc program args fallback =
    catchIOError (procs program args empty) $ \ ex ->
        if isDoesNotExistError ex
        then fallback
        else ioError ex

-- | Invoke a process if the given 'program' exists.
-- Print some error message and then terminate with an exit code 1
-- if it does not exist.
proc' :: T.Text -> [T.Text] -> IO ()
proc' program args =
    tryProc program args $ do
        err ""
        err [qq|Couldn't find executable: "$program".|]
        exit $ ExitFailure 1

tryProcEnv :: String -> T.Text -> [T.Text] -> IO () -> IO ()
tryProcEnv envVar program args fallback = do
    env <- lookupEnv envVar
    let prog = maybe program T.pack env
    tryProc prog args fallback

procEnv :: String -> T.Text -> [T.Text] -> IO ()
procEnv envVar program args =
    tryProcEnv envVar program args $ do
        err ""
        err [qq|Couldn't find executable: "$program".|]
        err [qq|(You can explicitly set $envVar to locate the "$program".)|]
        exit $ ExitFailure 1

-- | Invoke a 'nirum' compiler.
nirum :: [T.Text] -> IO ()
nirum args = do
    nirumEnv <- lookupEnv "NIRUM"
    case nirumEnv of
        Just prog -> proc' (T.pack prog) args
        Nothing -> tryProc "stack" (["exec", "--", "nirum"] ++ args) $
                           proc' "nirum" args

-- | Run the IO operation only if it's run on CI.
whenCi :: IO () -> IO ()
whenCi block = do
    ciEnv <- lookupEnv "CI"
    let ci = maybe False (/= "") ciEnv
    when ci block

-- | Teget all targets.
main :: IO ()
main = do
    -- CHECK: If an added test suite requires any external programs
    -- list them in "External dependencies" section of CONTRIBUTING.md docs.
    elm
    python

-- | Test Elm target.
elm :: IO ()
elm = withSystemTempDirectory "nirum-elm-test" $ \ tmpdir -> do
    let tmpdirT = T.pack tmpdir
    let tmpDirFP = fromString tmpdir :: Turtle.FilePath
    nirum ["-t", "elm", "-o", tmpdirT, "examples/"]
    back <- pwd
    cd tmpDirFP
    tryProcEnv "ELM_INSTALL" "elm-install" [] $
        procEnv "ELM_PACKAGE" "elm-package" ["install"]
    procEnv "ELM_MAKE" "elm-make" []
    cd back

-- | Test Python target.
python :: IO ()
python = do
    toxEnv <- lookupEnv "TOX"
    let tox = procEnv "TOX" "tox"
    whenCi $ tox ["-e", "devruntime"]
    tox ["--skip-missing-interpreters"]
