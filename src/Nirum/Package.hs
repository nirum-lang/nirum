{-# LANGUAGE RankNTypes, StandaloneDeriving #-}
module Nirum.Package ( BoundModule (boundPackage, modulePath)
                     , Package (Package, metadata, modules)
                     , PackageError ( ImportError
                                    , MetadataError
                                    , ParseError
                                    , ScanError
                                    )
                     , TypeLookup (Imported, Local, Missing)
                     , docs
                     , findInBoundModule
                     , lookupType
                     , resolveBoundModule
                     , resolveModule
                     , scanModules
                     , scanPackage
                     , target
                     , types
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

import Nirum.Constructs.Docs (Docs)
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Identifier (Identifier)
import qualified Nirum.Constructs.Module as Mod
import Nirum.Constructs.ModulePath (ModulePath, fromFilePath)
import Nirum.Constructs.TypeDeclaration ( Type
                                        , TypeDeclaration ( Import
                                                          , ServiceDeclaration
                                                          , TypeDeclaration
                                                          , type'
                                                          )
                                        )
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

resolveModule :: ModulePath -> Package t -> Maybe Mod.Module
resolveModule path Package { modules = ms } = MS.lookup path ms

resolveBoundModule :: ModulePath -> Package t -> Maybe (BoundModule t)
resolveBoundModule path package =
    case resolveModule path package of
        Just _ -> Just $ BoundModule package path
        Nothing -> Nothing

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
                       -> Either ParseError Mod.Module
                       -> Either PackageError (M.Map ModulePath Mod.Module)
                       -> Either PackageError (M.Map ModulePath Mod.Module)
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

data BoundModule t = BoundModule { boundPackage :: Target t => Package t
                                 , modulePath :: ModulePath
                                 }

deriving instance (Eq t, Target t) => Eq (BoundModule t)
deriving instance (Ord t, Target t) => Ord (BoundModule t)
deriving instance (Show t, Target t) => Show (BoundModule t)

findInBoundModule :: Target t => (Mod.Module -> a) -> a -> BoundModule t -> a
findInBoundModule valueWhenExist valueWhenNotExist
                  BoundModule { boundPackage = Package { modules = ms }
                              , modulePath = path
                              } =
    case MS.lookup path ms of
        Nothing -> valueWhenNotExist
        Just mod' -> valueWhenExist mod'

types :: Target t => BoundModule t -> DS.DeclarationSet TypeDeclaration
types = findInBoundModule Mod.types DS.empty

docs :: Target t => BoundModule t -> Maybe Docs
docs = findInBoundModule Mod.docs Nothing

data TypeLookup = Missing
                | Local Type
                | Imported ModulePath Type
                deriving (Eq, Ord, Show)

lookupType :: Target t => Identifier -> BoundModule t -> TypeLookup
lookupType identifier boundModule =
    case DS.lookup identifier (types boundModule) of
        Nothing -> toType Mod.coreModulePath
            (DS.lookup identifier $ Mod.types Mod.coreModule)
        Just TypeDeclaration { type' = t } -> Local t
        Just (Import path _ _) ->
            case resolveModule path (boundPackage boundModule) of
                Nothing -> Missing
                Just (Mod.Module decls _) ->
                    toType path (DS.lookup identifier decls)
        Just ServiceDeclaration {} -> Missing
  where
    toType :: ModulePath -> Maybe TypeDeclaration -> TypeLookup
    toType mp (Just TypeDeclaration { type' = t }) = Imported mp t
    toType _ _ = Missing
