module Nirum.Package
    ( Package (..)
    , PackageError (..)
    , docs
    , resolveModule
    , scanModules
    , scanPackage
    , target
    ) where

import System.IO.Error (catchIOError)

import Control.Monad.Except ( ExceptT
                            , MonadError (throwError)
                            , liftIO
                            , runExceptT
                            )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

import Nirum.Constructs.Module
import Nirum.Constructs.ModulePath (ModulePath, fromFilePath)
import Nirum.Package.Metadata ( MetadataError
                              , Package (Package, metadata, modules)
                              , Target
                              , metadataPath
                              , packageTarget
                              , readFromPackage
                              )
import qualified Nirum.Package.ModuleSet as MS
import Nirum.Parser (ParseError, parseFile)

target :: Target t => Package t -> t
target = packageTarget

resolveModule :: ModulePath -> Package t -> Maybe Module
resolveModule path Package { modules = ms } = MS.lookup path ms

data PackageError = ScanError FilePath IOError
                  | ParseError ModulePath ParseError
                  | ImportError (S.Set MS.ImportError)
                  | MetadataError MetadataError
                  deriving (Eq, Show)

-- | Scan the given package path, and then return the read package.
scanPackage :: Target t => FilePath -> IO (Either PackageError (Package t))
scanPackage packagePath = runExceptT $ do
    metadataE <- catch (readFromPackage packagePath)
                       (ScanError $ metadataPath packagePath)
    metadata' <- case metadataE of
        Right m -> return m
        Left e -> throwError $ MetadataError e
    modulePaths <- liftIO $ scanModules packagePath
    modules' <- mapM (\ p -> catch (parseFile p) $ ScanError p) modulePaths
    case M.foldrWithKey excludeFailedParse (Right M.empty) modules' of
        Right parsedModules -> case MS.fromMap parsedModules of
            Right ms -> return $ Package metadata' ms
            Left errors -> throwError $ ImportError errors
        Left error' -> throwError error'
  where
    excludeFailedParse :: ModulePath
                       -> Either ParseError Module
                       -> Either PackageError (M.Map ModulePath Module)
                       -> Either PackageError (M.Map ModulePath Module)
    excludeFailedParse _ _ (Left error') = Left error'
    excludeFailedParse path (Left error') _ = Left $ ParseError path error'
    excludeFailedParse path (Right module') (Right map') =
        Right (M.insert path module' map')
    catch :: IO a -> (IOError -> e) -> ExceptT e IO a
    catch op onError = do
        result <- liftIO $ catchIOError (fmap Right op)
                                        (return . Left . onError)
        case result of
            Left err -> throwError err
            Right val -> return val

-- | Scan the given path recursively, and then return the map of
-- detected module paths.
scanModules :: FilePath -> IO (M.Map ModulePath FilePath)
scanModules packagePath = do
    files <- scanFiles ""
    return $ M.fromList files
  where
    isNotHidden :: FilePath -> Bool
    isNotHidden ('.' : _) = False
    isNotHidden _ = True
    scanFiles :: FilePath -> IO [(ModulePath, FilePath)]
    scanFiles path = do
        dir <- doesDirectoryExist realPath
        if dir
        then do
            subpaths <- listDirectory realPath
            scans <- mapM (scanFiles . (path </>)) $ filter isNotHidden subpaths
            return $ concat scans
        else return $ case fromFilePath path of
                          Just modulePath' -> [(modulePath', realPath)]
                          Nothing -> []
      where
        realPath :: FilePath
        realPath = case path of
                       [] -> packagePath
                       p -> packagePath </> p
