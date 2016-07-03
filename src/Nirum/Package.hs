module Nirum.Package ( Package(Package, modules)
                     , scanModules
                     , scanPackage
                     ) where

import Data.Map.Strict (Map, empty, foldrWithKey, fromList, insert)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

import Nirum.Constructs.Module (Module)
import Nirum.Constructs.ModulePath (ModulePath, fromFilePath)
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
    files <- scanFiles ""
    return $ fromList files
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
