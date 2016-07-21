{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- | Inquiry to Nirum version.
module Nirum.Version (version, versionString, versionText) where

import Data.Maybe (mapMaybe)
import Data.Version (versionBranch, versionTags)

import qualified Data.SemVer as SV
import Data.Text (Text, pack)

import qualified Paths_nirum as P
-- Special module provided by Cabal
-- See also: http://stackoverflow.com/a/2892624/383405

-- | The semantic version of the running Nirum.
version :: SV.Version
version = case branch of
    [major, minor, patch] ->
        if length relTags == length tags
        then SV.version major minor patch relTags []
        else error ("invalid release identifiers: " ++ show relTags)
    [_, _] -> error ("patch version is missing: " ++ show branch)
    [_] -> error ("minor version is missing: " ++ show branch)
    [] -> error "major version is missing"
    _ -> error ("too precise version for semver: " ++ show branch)
  where
    branch :: [Int]
    branch = versionBranch P.version
    tags :: [String]
    tags = versionTags P.version
    relTags :: [SV.Identifier]
    relTags = mapMaybe (SV.textual . pack) tags

-- | The string representation of 'version'.
versionString :: String
versionString = SV.toString version

-- | The text representation of 'version'.
versionText :: Text
versionText = SV.toText version
