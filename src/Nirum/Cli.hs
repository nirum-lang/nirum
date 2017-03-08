{-# LANGUAGE ExtendedDefaultRules, QuasiQuotes, DeriveDataTypeable #-}
module Nirum.Cli (main, writeFiles) where

import Control.Monad (forM_)
import GHC.Exts (IsList (toList))
import System.IO.Error (catchIOError, ioeGetErrorString)

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Console.CmdArgs.Implicit ( Data
                                       , Typeable
                                       , argPos
                                       , cmdArgs
                                       , explicit
                                       , help
                                       , name
                                       , program
                                       , summary
                                       , typ
                                       , typDir
                                       , (&=)
                                       )
import System.Console.CmdArgs.Default (def)
import System.Directory (createDirectoryIfMissing)
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

data NirumCli = NirumCli { sourcePath :: FilePath
                         , objectPath :: FilePath
                         , targetName :: TargetName
                         } deriving (Show, Data, Typeable)

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

nirumCli :: NirumCli
nirumCli = NirumCli
    { objectPath = def &= explicit
          &= name "o" &= name "output-dir" &= typDir
          &= help "The directory to place object files"
    , targetName = "python" &= explicit
          &= name "t" &= name "target" &= typ "TARGET"
          &= help ("The target language.  Available targets: " ++
                   T.unpack targetNamesText)
    , sourcePath = def &= argPos 1 &= typDir
    } &= program "nirum" &= summary ("Nirum Compiler " ++ versionString)

targetNamesText :: T.Text
targetNamesText = T.intercalate ", " $ S.toAscList targetNames

main' :: IO ()
main' = do
    NirumCli src outDir target <- cmdArgs nirumCli
    result <- buildPackage target src
    case result of
        Left (TargetNameError targetName') ->
            putStrLn [qq|$targetName': No such target.
Available targets: $targetNamesText|]
        Left (PackageError (ParseError modulePath error')) -> do
            {- FIXME: find more efficient way to determine filename from
                      the given module path -}
            filePaths <- scanModules src
            case M.lookup modulePath filePaths of
                Just filePath' -> do
                    m <- parseErrortoPrettyMessage error' filePath'
                    putStrLn m
                Nothing -> putStrLn [qq|$modulePath not found|]
        Left (PackageError (ImportError importErrors)) ->
            putStrLn [qq|Import error:
{importErrorsToPrettyMessage importErrors}
|]
        Left (PackageError (ScanError _ error')) ->
            putStrLn [qq|Scan error: $error'|]
        Left (PackageError (MetadataError error')) ->
            putStrLn [qq|Metadata error: $error'|]
        Left (CompileError errors) ->
            forM_ (M.toList errors) $ \ (filePath, compileError) ->
                putStrLn [qq|error: $filePath: $compileError|]
        Right buildResult -> writeFiles outDir buildResult

writeFiles :: FilePath -> BuildResult -> IO ()
writeFiles outDir files =
    forM_ (M.toAscList files) $ \ (filePath, code) -> do
        let outPath = outDir </> filePath
        createDirectoryIfMissing True $ takeDirectory outPath
        putStrLn outPath
        B.writeFile outPath code

main :: IO ()
main = catchIOError main' $ putStrLn . ioeGetErrorString
