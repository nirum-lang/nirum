module Nirum.Package ( BoundModule(boundPackage, modulePath)
                     , ImportError ( CircularImportError
                                   , MissingImportError
                                   , MissingModulePathError
                                   )
                     , MetadataError ( FieldError
                                     , FieldTypeError
                                     , FieldValueError
                                     , FormatError
                                     )
                     , MetadataField
                     , MetadataFieldType
                     , Package (modules, version)
                     , PackageError ( ImportError
                                    , MetadataError
                                    , ParseError
                                    , ScanError
                                    )
                     , TypeLookup (Imported, Local, Missing)
                     , docs
                     , findInBoundModule
                     , lookupType
                     , makePackage
                     , metadataFilename
                     , resolveBoundModule
                     , resolveModule
                     , scanModules
                     , scanPackage
                     , types
                     ) where

import System.IO.Error (catchIOError)

import Control.Monad.Except ( ExceptT
                            , MonadError(throwError)
                            , liftIO
                            , runExceptT
                            )
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.SemVer as SV
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import qualified Text.Parsec.Error as PE
import Text.Toml (parseTomlDoc)
import Text.Toml.Types (Node ( VArray
                             , VBoolean
                             , VDatetime
                             , VFloat
                             , VInteger
                             , VString
                             , VTable
                             , VTArray
                             )
                       , Table
                       )

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
import Nirum.Parser (ParseError, parseFile)

-- | The filename of Nirum package metadata.
metadataFilename :: FilePath
metadataFilename = "package.toml"

-- | Represents a package which consists of modules.
data Package = Package { version :: SV.Version
                       , modules :: M.Map ModulePath Mod.Module
                       } deriving (Eq, Ord, Show)
-- TODO: uri, dependencies

data ImportError = CircularImportError [ModulePath]
                 | MissingModulePathError ModulePath ModulePath
                 | MissingImportError ModulePath ModulePath Identifier
                 deriving (Eq, Ord, Show)

makePackage :: SV.Version
            -> M.Map ModulePath Mod.Module
            -> Either (S.Set ImportError) Package
makePackage version' modules'
    | S.null importErrors = Right package
    | otherwise = Left importErrors
  where
    package :: Package
    package = Package version' modules'
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
                        Just ServiceDeclaration {} -> []
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
                  | MetadataError MetadataError
                  deriving (Eq, Show)

type MetadataField = T.Text
type MetadataFieldType = T.Text

data MetadataError
    = FieldError MetadataField
    | FieldTypeError MetadataField MetadataFieldType MetadataFieldType
    | FieldValueError MetadataField String
    | FormatError PE.ParseError
    deriving (Eq, Show)

-- | Scan the given package path, and then return the read package.
scanPackage :: FilePath -> IO (Either PackageError Package)
scanPackage packagePath = runExceptT $ do
    metadataText <- catch (TI.readFile metadataPath) $ ScanError metadataPath
    table <- case parseTomlDoc metadataPath metadataText of
        Left e -> throwError $ MetadataError $ FormatError e
        Right t -> return t
    version' <- versionField "version" table
    modulePaths <- liftIO $ scanModules packagePath
    modules' <- mapM (\p -> catch (parseFile p) $ ScanError p) modulePaths
    case M.foldrWithKey excludeFailedParse (Right M.empty) modules' of
        Right parsedModules -> case makePackage version' parsedModules of
            Right p -> return p
            Left errors -> throwError $ ImportError errors
        Left error' -> throwError error'
  where
    metadataPath :: FilePath
    metadataPath = packagePath </> metadataFilename
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
    show' :: Show a => a -> T.Text
    show' = T.pack . show
    printNode :: Node -> MetadataFieldType
    printNode (VTable t) = T.concat ["table of ", show' (length t), " items"]
    printNode (VTArray a) = T.concat ["array of ", show' (length a), " tables"]
    printNode (VString s) = T.concat ["string (", show' s, ")"]
    printNode (VInteger i) = T.concat ["integer (", show' i, ")"]
    printNode (VFloat f) = T.concat ["float (", show' f, ")"]
    printNode (VBoolean True) = "boolean (true)"
    printNode (VBoolean False) = "boolean (false)"
    printNode (VDatetime d) = T.concat ["datetime (", show' d, ")"]
    printNode (VArray a) = T.concat ["array of ", show' (length a), " values"]
    field :: MetadataField -> Table -> ExceptT PackageError IO Node
    field field' table =
        case HM.lookup field' table of
            Just node -> return node
            Nothing -> throwError $ MetadataError $ FieldError field'
    typedField :: MetadataFieldType
               -> (Node -> Maybe v)
               -> MetadataField
               -> Table
               -> ExceptT PackageError IO v
    typedField typename match field' table = do
        node <- field field' table
        case match node of
            Just value -> return value
            Nothing -> let error' = FieldTypeError field' typename $
                                                   printNode node
                       in throwError $ MetadataError error'
    stringField :: MetadataField -> Table -> ExceptT PackageError IO T.Text
    stringField = typedField "string" $ \n -> case n of
                                                   VString s -> Just s
                                                   _ -> Nothing
    versionField :: MetadataField -> Table -> ExceptT PackageError IO SV.Version
    versionField field' table = do
        s <- stringField field' table
        case SV.fromText s of
            Right v -> return v
            Left _ ->
                let msg = "expected a semver string (e.g. \"1.2.3\"), not " ++
                          show s
                in throwError $ MetadataError $ FieldValueError field' msg

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
