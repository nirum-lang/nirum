{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings, QuasiQuotes #-}
module Nirum.Cli (main) where

import System.Environment (getArgs, getProgName)

import qualified Data.Text.IO as TI
import Text.InterpolatedString.Perl6 (qq)
import Text.Megaparsec.Error (ParseError, errorMessages, showMessages)

import Nirum.Parser (parseFile)
import Nirum.Targets.Python (compileModule)

showError :: ParseError -> String
showError = showMessages . errorMessages

main :: IO ()
main = do
    prog <- getProgName
    args <- getArgs
    case args of
        inputFilename : outputFilename : [] -> do
            result <- parseFile inputFilename
            case result of
                Left error' -> putStrLn [qq|$prog: error: {showError error'}|]
                Right module' -> do
                    let code = compileModule module'
                    TI.writeFile outputFilename code
        [] -> putStrLn [qq|$prog: too few arguments|]
        _ -> putStrLn [qq|$prog: too many arguments|]
