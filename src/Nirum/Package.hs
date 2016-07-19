module Nirum.Package ( BoundModule(boundPackage, modulePath)
                     , ImportError ( CircularImportError
                                   , MissingImportError
                                   , MissingModulePathError
                                   )
                     , Package(modules)
                     , PackageError(ImportError, ParseError, ScanError)
                     , TypeLookup(Imported, Local, Missing)
                     , docs
                     , findInBoundModule
                     , lookupType
                     , makePackage
                     , resolveBoundModule
                     , resolveModule
                     , scanModules
                     , scanPackage
                     , types
                     ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

import Nirum.Constructs.Declaration (Docs)
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Identifier (Identifier)
import qualified Nirum.Constructs.Module as Mod
import Nirum.Constructs.ModulePath (ModulePath, fromFilePath)
import Nirum.Constructs.TypeDeclaration ( Type
                                        , TypeDeclaration ( Import
                                                          , TypeDeclaration
                                                          , type'
                                                          )
                                        )
import Nirum.Parser (ParseError, parseFile)

-- | Represents a package which consists of modules.
data Package = Package { modules :: M.Map ModulePath Mod.Module
                       } deriving (Eq, Ord, Show)
-- TODO: uri, version, dependencies

data ImportError = CircularImportError [ModulePath]
                 | MissingModulePathError ModulePath ModulePath
                 | MissingImportError ModulePath ModulePath Identifier
                 deriving (Eq, Ord, Show)

makePackage :: M.Map ModulePath Mod.Module -> Either (S.Set ImportError) Package
makePackage modules'
    | S.null importErrors = Right package
    | otherwise = Left importErrors
  where
    package :: Package
    package = Package modules'
    importErrors :: S.Set ImportError
    importErrors = detectImportErrors package

resolveModule :: ModulePath -> Package -> Maybe Mod.Module
resolveModule path Package { modules = ms } = M.lookup path ms

resolveBoundModule :: ModulePath -> Package -> Maybe BoundModule
resolveBoundModule path package =
    case resolveModule path package of
        Just _ -> Just $ BoundModule package path
        Nothing -> Nothing

detectImportErrors :: Package -> S.Set ImportError
detectImportErrors package = detectMissingImports package `S.union`
                             detectCircularImports package

detectMissingImports :: Package -> S.Set ImportError
detectMissingImports package@Package { modules = ms } =
    S.fromList [e | (path, module') <- M.toList ms, e <- detect path module']
  where
    detect :: ModulePath -> Mod.Module -> [ImportError]
    detect path module' =
        [ e
        | (path', idents) <- M.toList (Mod.imports module')
        , e <- case resolveModule path' package of
                Nothing -> [MissingModulePathError path path']
                Just (Mod.Module decls _) ->
                    [ e
                    | i <- S.toList idents
                    , e <- case DS.lookup i decls of
                        Just TypeDeclaration {} -> []
                        Just Import {} -> [MissingImportError path path' i]
                        Nothing -> [MissingImportError path path' i]
                    ]
        ]

detectCircularImports :: Package -> S.Set ImportError
detectCircularImports Package { modules = ms } =
    S.fromList [e | path <- M.keys ms, e <- detect path []]
  where
    moduleImports :: M.Map ModulePath (S.Set ModulePath)
    moduleImports =
        M.fromList [ (path, M.keysSet $ Mod.imports module')
                   | (path, module') <- M.toList ms
                   ]
    detect :: ModulePath -> [ModulePath] -> [ImportError]
    detect path reversedCycle
        | path `elem` reversedCycle =
            [CircularImportError $ reverse reversedCycle']
        | otherwise =
            case M.lookup path moduleImports of
                Just paths -> [ e
                              | path' <- S.toList paths
                              , e <- detect path' reversedCycle'
                              ]
                Nothing -> []
      where
        reversedCycle' :: [ModulePath]
        reversedCycle' = path : reversedCycle

data PackageError = ScanError FilePath IOError
                  | ParseError ModulePath ParseError
                  | ImportError (S.Set ImportError)
                  deriving (Eq, Show)

-- | Scan the given package path, and then return the read package.
scanPackage :: FilePath -> IO (Either PackageError Package)
scanPackage packagePath = do
    modulePaths <- scanModules packagePath
    -- FIXME: catch IO errors
    modules' <- mapM parseFile modulePaths
    return $ case M.foldrWithKey excludeFailedParse (Right M.empty) modules' of
        Right parsedModules -> case makePackage parsedModules of
            Right p -> Right p
            Left errors -> Left $ ImportError errors
        Left error' -> Left error'
  where
    excludeFailedParse :: ModulePath
                       -> Either ParseError Mod.Module
                       -> Either PackageError (M.Map ModulePath Mod.Module)
                       -> Either PackageError (M.Map ModulePath Mod.Module)
    excludeFailedParse _ _ (Left error') = Left error'
    excludeFailedParse path (Left error') _ = Left $ ParseError path error'
    excludeFailedParse path (Right module') (Right map') =
        Right (M.insert path module' map')

-- | Scan the given path recursively, and then return the map of
-- detected module paths.
scanModules :: FilePath -> IO (M.Map ModulePath FilePath)
scanModules packagePath = do
    files <- scanFiles ""
    return $ M.fromList files
  where
    isNotHidden :: FilePath -> Bool
    isNotHidden ('.':_) = False
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

data BoundModule = BoundModule { boundPackage :: Package
                               , modulePath :: ModulePath
                               } deriving (Eq, Ord, Show)

findInBoundModule :: (Mod.Module -> a) -> a -> BoundModule -> a
findInBoundModule valueWhenExist valueWhenNotExist
                  BoundModule { boundPackage = Package { modules = ms }
                              , modulePath = path
                              } =
    case M.lookup path ms of
        Nothing -> valueWhenNotExist
        Just mod' -> valueWhenExist mod'

types :: BoundModule -> DS.DeclarationSet TypeDeclaration
types = findInBoundModule Mod.types DS.empty

docs :: BoundModule -> Maybe Docs
docs = findInBoundModule Mod.docs Nothing

data TypeLookup = Missing
                | Local Type
                | Imported ModulePath Type
                deriving (Eq, Ord, Show)

lookupType :: Identifier -> BoundModule -> TypeLookup
lookupType identifier boundModule =
    case DS.lookup identifier (types boundModule) of
        Nothing -> toType Mod.coreModulePath
            (DS.lookup identifier $ Mod.types Mod.coreModule)
        Just TypeDeclaration { type' = t } -> Local t
        Just (Import path _) ->
            case resolveModule path (boundPackage boundModule) of
                Nothing -> Missing
                Just (Mod.Module decls _) ->
                    toType path (DS.lookup identifier decls)
  where
    toType :: ModulePath -> Maybe TypeDeclaration -> TypeLookup
    toType mp (Just TypeDeclaration { type' = t }) = Imported mp t
    toType _ _ = Missing
