{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TemplateHaskell #-}
module Nirum.Targets
    ( BuildError (..)
    , BuildResult
    , Target (..)
    , TargetName
    , buildPackage
    , buildPackageFromFilePath
    , targetNames
    ) where

import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy)

import Data.ByteString (ByteString)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Text (Text)

import Nirum.Package (PackageError, scanPackage)
import Nirum.Package.Metadata ( Metadata (Metadata, target)
                              , Package (Package, metadata)
                              , Target ( CompileError
                                       , CompileResult
                                       , compilePackage
                                       , parseTarget
                                       , showCompileError
                                       , targetName
                                       , toByteString
                                       )
                              , TargetName
                              )
import Nirum.Targets.List (targetProxyMapQ)

-- Imported targets below are automatically added to the list of targets.
-- These target names become options of `[targets.*]` sections of package.toml
-- and `-t`/`--target` of CLI as well.
--
-- CHECK: When a new target `Nirum.Targets.X` is added, write docs for it in
-- docs/target/x.md file too.
import Nirum.Targets.Docs ()
import Nirum.Targets.Python ()

data BuildError = TargetNameError TargetName
                | CompileError (M.Map FilePath Text)
                | PackageError PackageError
                deriving (Eq, Show)
type BuildResult = M.Map FilePath ByteString

packageBuilders :: M.Map TargetName
                         (FilePath -> IO (Either BuildError BuildResult))
packageBuilders = M.fromList $(targetProxyMapQ [e|buildPackageFromFilePath'|])

targetNames :: Set TargetName
targetNames = M.keysSet packageBuilders

buildPackage :: forall t. Target t => Package t -> Either BuildError BuildResult
buildPackage pkg@Package { metadata = Metadata { target = target' } } =
    case partitionEithers eithers of
        (errors@(_ : _), _) ->
            Left $ CompileError $ M.fromList errors
        ([], outs) -> Right $ M.fromList outs
  where
    results :: [(FilePath, Either (CompileError t) (CompileResult t))]
    results = M.toList $ compilePackage pkg
    eithers :: [Either (FilePath, Text) (FilePath, ByteString)]
    eithers = [ case result of
                    Left e -> Left (f, showCompileError target' e)
                    Right r -> Right (f, toByteString target' r)
              | (f, result) <- results
              ]

buildPackageFromFilePath :: TargetName
                         -> FilePath
                         -> IO (Either BuildError BuildResult)
buildPackageFromFilePath targetName' =
    fromMaybe (\ _ -> return $ Left $ TargetNameError targetName') $
              M.lookup targetName' packageBuilders

buildPackageFromFilePath' :: forall t. Target t
                          => Proxy t
                          -> FilePath
                          -> IO (Either BuildError BuildResult)
buildPackageFromFilePath' _ packagePath = do
    scanResult <- scanPackage packagePath
    return $ case scanResult of
        Left e -> Left $ PackageError e
        Right (pkg :: Package t) ->
            buildPackage pkg
