{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings, QuasiQuotes,
             DeriveDataTypeable #-}
module Nirum.Cli (main, parseErrorToPrettyMessage, writeFiles) where

import Control.Monad (forM_)
import GHC.Exts (IsList(toList))
import System.IO.Error (catchIOError, ioeGetErrorString)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.Console.CmdArgs.Implicit ( Data
                                       , Typeable
                                       , argPos
                                       , cmdArgs
                                       , explicit
                                       , help
                                       , name
                                       , program
                                       , summary
                                       , typDir
                                       , versionArg
                                       , (&=)
                                       )
import System.Console.CmdArgs.Default (def)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Text.InterpolatedString.Perl6 (qq)
import Text.Megaparsec (Token)
import Text.Megaparsec.Error ( Dec
                             , ParseError(errorPos)
                             , parseErrorPretty
                             )
import Text.Megaparsec.Pos (SourcePos(sourceLine, sourceColumn), unPos)

import Nirum.Constructs (Construct(toCode))
import Nirum.Constructs.Identifier (toText)
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Package ( PackageError(ParseError, ImportError, ScanError)
                     , ImportError ( CircularImportError
                                   , MissingImportError
                                   , MissingModulePathError
                                   )
                     , scanModules
                     , scanPackage
                     )
import Nirum.Targets.Python (compilePackage)
import Nirum.Version (versionString)

data NirumCli = NirumCli { sourcePath :: FilePath
                         , objectPath :: FilePath
                         } deriving (Show, Data, Typeable)

parseErrorToPrettyMessage :: ParseError (Token T.Text) Dec
                          -> FilePath
                          -> IO String
parseErrorToPrettyMessage parseError' filePath' = do
    sourceCode <- readFile filePath'
    let sourceLines = lines sourceCode
        sl = if length sourceLines < errorLine then ""
             else sourceLines !! (errorLine - 1)
    return [qq|
{parseErrorPretty $ parseError'}
$sl
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

toModuleNameText :: T.Text -> T.Text
toModuleNameText t = [qq|'{t}'|]

modulePathToRepr :: ModulePath -> T.Text
modulePathToRepr = toModuleNameText . toCode

importErrorToPrettyMessage :: ImportError -> T.Text
importErrorToPrettyMessage (CircularImportError modulePaths) =
    [qq|Circular import detected in following orders: $order|]
  where
    circularModulesText :: [ModulePath] -> [T.Text]
    circularModulesText = map modulePathToRepr
    order :: T.Text
    order = T.intercalate " > " $ circularModulesText modulePaths
importErrorToPrettyMessage (MissingModulePathError path path') =
    [qq|No module named $dataName in $moduleName|]
  where
    moduleName :: T.Text
    moduleName = modulePathToRepr path
    dataName :: T.Text
    dataName = modulePathToRepr path'
importErrorToPrettyMessage (MissingImportError path path' identifier) =
    [qq|Cannot import $importText from $attrText in $foundText|]
  where
    importText :: T.Text
    importText = (toModuleNameText . toText) identifier
    foundText :: T.Text
    foundText = modulePathToRepr path
    attrText :: T.Text
    attrText = modulePathToRepr path'


importErrorsToMessageList :: S.Set ImportError -> [T.Text]
importErrorsToMessageList importErrors =
    S.toList $ S.map importErrorToPrettyMessage importErrors
importErrorsToPrettyMessage :: S.Set ImportError -> String
importErrorsToPrettyMessage importErrors =
    T.unpack $ T.intercalate "\n" withListStyleText
  where
    withListStyleText :: [T.Text]
    withListStyleText =
        map (T.append "- ") (importErrorsToMessageList importErrors)

nirumCli :: NirumCli
nirumCli = NirumCli { objectPath = def &= explicit
                          &= name "output-dir" &= typDir
                          &= help "The directory to place object files"
                    , sourcePath = def &= argPos 1 &= typDir
                    }
         &= program "nirum"
         &= summary ("Nirum Compiler " ++ versionString)
         &= versionArg [summary versionString]

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
                    m <- parseErrorToPrettyMessage error' filePath'
                    putStrLn m
                Nothing -> putStrLn [qq|$modulePath not found|]
        Left (ImportError importErrors) ->
            putStrLn [qq|Import error:
{importErrorsToPrettyMessage importErrors}
|]
        Left (ScanError _ error') -> putStrLn [qq|Scan error: $error'|]
        Right pkg -> writeFiles obj $ compilePackage pkg

writeFiles :: FilePath -> M.Map FilePath (Either T.Text T.Text) -> IO ()
writeFiles obj m =
    forM_ files $ \(filePath, result) ->
        case result of
            Left compileError -> putStrLn [qq|error: $filePath: $compileError|]
            Right code -> do
                createDirectoryIfMissing True $ takeDirectory filePath
                putStrLn filePath
                TI.writeFile filePath code
  where
    files :: [(FilePath, Either T.Text T.Text)]
    files = [(obj </> f, r) | (f, r) <- M.toList m]

main :: IO ()
main = catchIOError main' $ putStrLn . ioeGetErrorString
