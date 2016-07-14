{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings, QuasiQuotes,
             DeriveDataTypeable #-}
module Nirum.Cli (main) where

import Control.Monad (forM_)
import GHC.Exts (IsList(toList))
import System.IO.Error (catchIOError, ioeGetErrorString)

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.Console.CmdArgs.Implicit ( Data
                                       , Typeable
                                       , argPos
                                       , cmdArgs
                                       , help
                                       , program
                                       , summary
                                       , typDir
                                       , typFile
                                       , (&=)
                                       )
import System.Console.CmdArgs.Default (def)
import System.FilePath ((</>))
import Text.InterpolatedString.Perl6 (qq)
import Text.Megaparsec (Token)
import Text.Megaparsec.Error ( Dec
                             , ParseError(errorPos)
                             , parseErrorPretty
                             )
import Text.Megaparsec.Pos (SourcePos(sourceLine, sourceColumn), unPos)

import Nirum.Package (PackageError(ParseError), scanModules, scanPackage)
import Nirum.Targets.Python (compilePackage)

data NirumCli = NirumCli { sourcePath :: FilePath
                         , objectPath :: FilePath
                         } deriving (Show, Data, Typeable)

toErrorMessage :: ParseError (Token T.Text) Dec -> FilePath -> IO String
toErrorMessage parseError' filePath' = do
    sourceCode <- readFile filePath'
    let sourceLines = lines sourceCode
    return [qq|
{parseErrorPretty $ parseError'}

{sourceLines !! (errorLine - 1)}
{arrow}
|]
  where
    error' :: SourcePos
    error' = head $ toList $ errorPos parseError'
    errorLine :: Int
    errorLine = fromEnum $ unPos $ sourceLine error'
    errorColumn :: Int
    errorColumn = fromEnum $ unPos $ sourceColumn error'
    arrow :: T.Text
    arrow = T.snoc (T.concat (replicate (errorColumn - 1) (T.pack " "))) '^'

nirumCli :: NirumCli
nirumCli = NirumCli { objectPath = def &= typFile
                          &= help "The directory to place object files"
                    , sourcePath = def &= argPos 1 &= typDir
                    }
         &= program "nirum"
         &= summary "Nirum Compiler CLI"

main' :: IO ()
main' = do
    NirumCli src obj <- cmdArgs nirumCli
    scanResult <- scanPackage src
    case scanResult of
        Left (ParseError modulePath error') -> do
            -- FIXME: find more efficient way to determine filename from
            --        the given module path
            filePaths <- scanModules src
            case M.lookup modulePath filePaths of
                Just filePath' -> do
                    m <- toErrorMessage error' filePath'
                    putStrLn m
                    putStrLn [qq|error: {parseErrorPretty error'}|]
                Nothing -> putStrLn [qq|error: $modulePath|]
        Left error' -> putStrLn [qq|error: $error'|]
        Right pkg -> writeFiles obj $ compilePackage pkg

writeFiles :: FilePath -> M.Map FilePath (Either T.Text T.Text) -> IO ()
writeFiles obj m =
    forM_ files $ \(filePath, result) ->
        case result of
            Left compileError -> putStrLn [qq|error: $filePath: $compileError|]
            Right code -> do
                putStrLn filePath
                TI.writeFile filePath code
  where
    files :: [(FilePath, Either T.Text T.Text)]
    files = [(obj </> f, r) | (f, r) <- M.toList m]

main :: IO ()
main = catchIOError main' $ putStrLn . ioeGetErrorString
