module Nirum.Package ( Package(Package, modules)
                     , scanModules
                     , scanPackage
                     ) where

import Data.Char (toLower)
import Data.Maybe (mapMaybe)

import Data.Map.Strict (Map, empty, foldrWithKey, fromList, insert)
import Data.Text (pack)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

import Nirum.Constructs.Identifier (Identifier, fromText)
import Nirum.Constructs.Module (Module)
import Nirum.Constructs.ModulePath (ModulePath(ModuleName, ModulePath))
import Nirum.Parser (ParseError, parseFile)

-- | Represents a package which consists of modules.
data Package = Package { modules :: Map ModulePath Module
                       } deriving (Eq, Ord, Show)
-- TODO: uri, version, dependencies

-- | Scan the given package path, and then return the read package.
scanPackage :: FilePath -> IO (Either ParseError Package)
scanPackage packagePath = do
    modulePaths <- scanModules packagePath
    modules' <- mapM parseFile modulePaths
    return $ case foldrWithKey excludeFailedParse (Right empty) modules' of
        Right parsedModules -> Right Package { modules = parsedModules }
        Left parseError -> Left parseError
  where
    excludeFailedParse :: ModulePath
                       -> Either ParseError Module
                       -> Either ParseError (Map ModulePath Module)
                       -> Either ParseError (Map ModulePath Module)
    excludeFailedParse _ _ result@(Left _) = result
    excludeFailedParse _ (Left parseError) _ = Left parseError
    excludeFailedParse path (Right module') (Right map') =
        Right (insert path module' map')
                 

-- | Scan the given path recursively, and then return the map of
-- detected module paths.
scanModules :: FilePath -> IO (Map ModulePath FilePath)
scanModules packagePath = do
    files <- scanFiles packagePath Nothing
    return $ fromList files
  where
    scanFiles :: FilePath -> Maybe ModulePath -> IO [(ModulePath, FilePath)]
    scanFiles path prefix = do
        dir <- doesDirectoryExist path
        if dir
        then do subpaths <- listDirectory path
                filesList <- mapM
                    (\(p, m) -> scanFiles (path </> p) $
                                          Just $ submodule prefix m)
                    (mapMaybe toModuleName subpaths)
                return [(m, f) | files <- filesList, (m, f) <- files]
        else if map toLower (takeExtension path) == ".nrm"
             then return $ case prefix of
                               Just prefix' -> [(prefix', path)]
                               Nothing -> []
             else return []
    toModuleName :: FilePath -> Maybe (FilePath, Identifier)
    toModuleName f = case fromText (pack $ takeBaseName f) of
                         Just ident -> Just (f, ident)
                         Nothing -> Nothing
    submodule :: Maybe ModulePath -> Identifier -> ModulePath
    submodule Nothing = ModuleName
    submodule (Just path) = ModulePath path
