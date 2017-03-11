{-# LANGUAGE ExtendedDefaultRules, QuasiQuotes #-}
module Nirum.Cli (main, writeFiles) where

import GHC.Exts (IsList (toList))

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, newTVar, readTVar, TVar, writeTVar)
import Control.Monad (forM_, forever, when)
import Data.Monoid ((<>))
import qualified Options.Applicative as OPT
import System.Directory (createDirectoryIfMissing)
import System.Exit (die)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.FSNotify
import System.IO
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

type TFlag = TVar Bool
type Nanosecond = Int

data Opts = Opts { outDirectory :: !String
                 , targetOption :: !String
                 , watch :: !Bool
                 , packageDirectory :: !String
                 }

data AppOptions = AppOptions { outputPath :: FilePath
                             , packagePath :: FilePath
                             , targetLanguage :: T.Text
                             , watching :: Bool
                             , building :: TFlag
                             , changed :: TFlag
                             }

debounceDelay :: Nanosecond
debounceDelay = 1 * 1000 * 1000

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

build :: AppOptions -> IO ()
build options@AppOptions { packagePath = src
                         , outputPath = outDir
                         , targetLanguage = target
                         } = do
    result <- buildPackage target src
    case result of
        Left (TargetNameError targetName') ->
            tryDie' [qq|Couldn't find "$targetName'" target.
Available targets: $targetNamesText|]
        Left (PackageError (ParseError modulePath error')) -> do
            {- FIXME: find more efficient way to determine filename from
                      the given module path -}
            filePaths <- scanModules src
            case M.lookup modulePath filePaths of
                Just filePath' -> do
                    m <- parseErrortoPrettyMessage error' filePath'
                    tryDie' m
                Nothing -> tryDie' [qq|$modulePath not found|]
        Left (PackageError (ImportError importErrors)) ->
            tryDie' [qq|Import error:
{importErrorsToPrettyMessage importErrors}
|]
        Left (PackageError (ScanError _ error')) ->
            tryDie' [qq|Scan error: $error'|]
        Left (PackageError (MetadataError error')) ->
            tryDie' [qq|Metadata error: $error'|]
        Left (CompileError errors) ->
            forM_ (M.toList errors) $ \ (filePath, compileError) ->
                tryDie' [qq|error: $filePath: $compileError|]
        Right buildResult -> writeFiles outDir buildResult
  where
    tryDie' = tryDie options

writeFiles :: FilePath -> BuildResult -> IO ()
writeFiles outDir files =
    forM_ (M.toAscList files) $ \ (filePath, code) -> do
        let outPath = outDir </> filePath
        createDirectoryIfMissing True $ takeDirectory outPath
        putStrLn outPath
        B.writeFile outPath code


onFileChanged :: AppOptions -> Event -> IO ()
onFileChanged
    options@AppOptions { building = building'
                       , changed = changed'
                       }
                       event
  | takeExtension path == ".nrm" = do
        atomically $ writeTVar changed' True
        buildable <- atomically $ do
            b <- readTVar building'
            writeTVar building' True
            return $ not b
        when buildable $ do
            threadDelay debounceDelay
            reactiveBuild options
  | otherwise = return ()
  where
    path :: FilePath
    path = eventPath event

reactiveBuild :: AppOptions -> IO ()
reactiveBuild options@AppOptions { building = building'
                                 , changed = changed'
                                 } = do
    changed'' <- atomically $ readTVar changed'
    when changed'' $ do
        atomically $ writeTVar changed' False
        build options
        atomically $ writeTVar building' False
        changedDuringBuild <- atomically $ readTVar changed'
        when changedDuringBuild $ reactiveBuild options

tryDie :: AppOptions -> String -> IO ()
tryDie AppOptions { watching = watching' } errorMessage
  | watching' = hPutStrLn stderr errorMessage
  | otherwise = die errorMessage

main :: IO ()
main =
  withManager $ \ mgr -> do
    opts <- OPT.execParser optsParser
    building' <- atomically $ newTVar False
    changed' <- atomically $ newTVar True
    let watch' = watch opts
        packagePath' = packageDirectory opts
        options = AppOptions
            { outputPath = outDirectory opts
            , packagePath = packagePath'
            , targetLanguage = T.pack $ targetOption opts
            , watching = watch'
            , building = building'
            , changed = changed'
            }

    when watch' $ do
        _ <- watchDir mgr packagePath' (const True) (onFileChanged options)
        return ()
    reactiveBuild options
    -- sleep forever (until interrupted)
    when watch' $ forever $ threadDelay 1000000
  where
    optsParser :: OPT.ParserInfo Opts
    optsParser =
        OPT.info
            (OPT.helper <*> versionOption <*> programOptions)
            (OPT.fullDesc <>
             OPT.progDesc ("Nirum compiler " ++ versionString) <>
             OPT.header header)
    header :: String
    header = "Nirum: The IDL compiler and RPC/distributed object framework"
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
             OPT.help [qq|Target language name.
                          Available: $targetNamesText|]) <*>
        OPT.switch
            (OPT.long "watch" <> OPT.short 'w' <>
             OPT.help "Watch files for change and rebuild") <*>
        OPT.strArgument
            (OPT.metavar "DIR" <> OPT.help "Package directory")
