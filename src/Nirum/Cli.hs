{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings, QuasiQuotes,
             DeriveDataTypeable #-}
module Nirum.Cli (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.Console.CmdArgs.Implicit ( Data
                                       , Typeable
                                       , argPos
                                       , cmdArgs
                                       , enum
                                       , help
                                       , program
                                       , summary
                                       , typ
                                       , typFile
                                       , (&=)
                                       )
import System.Console.CmdArgs.Default (def)
import System.FilePath ( replaceDirectory
                       , replaceExtension
                       )
import Text.InterpolatedString.Perl6 (qq)
import Text.Megaparsec.Error ( ParseError
                             , errorMessages
                             , errorPos
                             , showMessages
                             )
import Text.Megaparsec.Pos (SourcePos(sourceName, sourceLine, sourceColumn))

import Nirum.Parser (parseFile)
import Nirum.Targets.Python (compileModule)

data TargetLanguage = Python
                    deriving (Show, Data)
data NirumCli = NirumCli { targetLanguage :: TargetLanguage
                         , nirumSource :: FilePath
                         , destination :: FilePath
                         } deriving (Show, Data, Typeable)

toErrorMessage :: ParseError -> FilePath -> IO String
toErrorMessage parseError' filePath' = do
    sourceCode <- readFile filePath'
    let sourceLine' = lines sourceCode
    return [qq|
File: "{sourceName error'}", line $errorLine column $errorColumn

{(!!) sourceLine' (errorLine - 1)}
{arrow}

{showMessages . errorMessages $ parseError'}|]
  where
    error' :: SourcePos
    error' = errorPos parseError'
    errorLine :: Int
    errorLine = sourceLine error'
    errorColumn :: Int
    errorColumn = sourceColumn error'
    arrow :: T.Text
    arrow = T.snoc (T.concat (replicate (errorColumn - 1) (T.pack " "))) '^'

nirumcli :: NirumCli
nirumcli = NirumCli { targetLanguage = enum [ Python &= help "python" ]
                    , destination = def &= typ "DEST"
                          &= help "Write generated file to DEST"
                    , nirumSource = def &= argPos 1 &= typFile
                    }
         &= program "nirum"
         &= summary "Nirum Compiler CLI"

main :: IO ()
main = do
    argument <- cmdArgs nirumcli
    let nirumSourceFilePath = nirumSource argument
    case targetLanguage argument of
        Python -> do
            result <- parseFile nirumSourceFilePath
            case result of
              Left error' -> do
                  m <- toErrorMessage error' nirumSourceFilePath
                  putStrLn m
              Right module' -> do
                  let code = compileModule module'
                      outputFilename = toOutputFilename nirumSourceFilePath $
                          destination argument
                  TI.writeFile outputFilename code
  where
    toOutputFilename :: FilePath -> FilePath -> String
    toOutputFilename source dest =
        if null dest
            then replaceExtension source ext
            else replaceDirectory (replaceExtension source ext) dest
      where
        ext = ".py"
