{-# LANGUAGE ExtendedDefaultRules, QuasiQuotes, DeriveDataTypeable #-}
module Nirum.Cli (main, writeFiles) where

import Control.Monad (forM_)
import GHC.Exts (IsList (toList))

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Monoid
import qualified Options.Applicative as OPT
import System.Directory (createDirectoryIfMissing)
import System.Exit (die)
import System.FilePath (takeDirectory, (</>))
import Text.InterpolatedString.Perl6 (qq)
import Text.Megaparsec (Token)
import Text.Megaparsec.Error ( Dec
                             , ParseError (errorPos)
                             , parseErrorPretty
                             )
import Text.Megaparsec.Pos (SourcePos (sourceLine, sourceColumn), unPos)

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Identifier (toText)
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Package ( PackageError ( ImportError
                                    , MetadataError
                                    , ParseError
                                    , ScanError
                                    )
                     , scanModules
                     )
import Nirum.Package.Metadata (TargetName)
import Nirum.Package.ModuleSet ( ImportError ( CircularImportError
                                             , MissingImportError
                                             , MissingModulePathError
                                             )
                               )
import Nirum.Targets ( BuildError (CompileError, PackageError, TargetNameError)
                     , BuildResult
                     , buildPackage
                     , targetNames
                     )
import Nirum.Version (versionString)

parseErrortoPrettyMessage :: ParseError (Token T.Text) Dec
                          -> FilePath
                          -> IO String
parseErrortoPrettyMessage parseError' filePath' = do
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

targetNamesText :: T.Text
targetNamesText = T.intercalate ", " $ S.toAscList targetNames

runCli :: FilePath -> FilePath -> TargetName -> IO ()
runCli src outDir target = do
    result <- buildPackage target src
    case result of
        Left (TargetNameError targetName') ->
            die [qq|Couldn't find "$targetName'" target.
Available targets: $targetNamesText|]
        Left (PackageError (ParseError modulePath error')) -> do
            {- FIXME: find more efficient way to determine filename from
                      the given module path -}
            filePaths <- scanModules src
            case M.lookup modulePath filePaths of
                Just filePath' -> do
                    m <- parseErrortoPrettyMessage error' filePath'
                    die m
                Nothing -> die [qq|$modulePath not found|]
        Left (PackageError (ImportError importErrors)) ->
            die [qq|Import error:
{importErrorsToPrettyMessage importErrors}
|]
        Left (PackageError (ScanError _ error')) ->
            die [qq|Scan error: $error'|]
        Left (PackageError (MetadataError error')) ->
            die [qq|Metadata error: $error'|]
        Left (CompileError errors) ->
            forM_ (M.toList errors) $ \ (filePath, compileError) ->
                die [qq|error: $filePath: $compileError|]
        Right buildResult -> writeFiles outDir buildResult

writeFiles :: FilePath -> BuildResult -> IO ()
writeFiles outDir files =
    forM_ (M.toAscList files) $ \ (filePath, code) -> do
        let outPath = outDir </> filePath
        createDirectoryIfMissing True $ takeDirectory outPath
        putStrLn outPath
        B.writeFile outPath code

data Opts = Opts { outDirectory :: !String
                 , targetOption :: !String
                 , packageDirectory :: !String
                 }

main :: IO ()
main = do
    opts <- OPT.execParser optsParser
    let packageDirectoryPath = packageDirectory opts
        outDirectoryPath = outDirectory opts
        targetName = T.pack $ targetOption opts
    runCli packageDirectoryPath outDirectoryPath targetName
  where
    optsParser :: OPT.ParserInfo Opts
    optsParser =
        OPT.info
            (OPT.helper <*> versionOption <*> programOptions)
            (OPT.fullDesc <>
             OPT.progDesc ("Nirum compiler" ++ versionString) <>
             OPT.header header)
    header :: String
    header = "nirum - The IDL compiler and RPC/distributed object framework"
    versionOption :: OPT.Parser (Opts -> Opts)
    versionOption = OPT.infoOption
        versionString (OPT.long "version" <>
                       OPT.short 'v' <> OPT.help "Show version")
    programOptions :: OPT.Parser Opts
    programOptions =
        Opts <$> OPT.strOption
            (OPT.long "output-dir" <> OPT.short 'o' <> OPT.metavar "DIR" <>
             OPT.help "Output directory") <*>
        OPT.strOption
            (OPT.long "target" <> OPT.short 't' <> OPT.metavar "TARGET" <>
             OPT.help "Target language name") <*>
        OPT.strArgument
            (OPT.metavar "DIR" <> OPT.help "Package directory")
