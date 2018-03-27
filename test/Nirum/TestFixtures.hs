module Nirum.TestFixtures (fixturePackage, scanFixturePackage) where

import Control.Monad
import Data.Either

import System.FilePath

import Nirum.Package
import Nirum.Package.Metadata


-- | Scan test/nirum_fixture/ package.
scanFixturePackage :: Target t => IO (Either PackageError (Package t))
scanFixturePackage = scanPackage $ "test" </> "nirum_fixture"

-- | Unsafe version of 'scanFixturePackage'.
fixturePackage :: Target t => IO (Package t)
fixturePackage = do
    result <- scanFixturePackage
    when (isLeft result) $ fail ("result: " ++ show result)
    let Right pkg = result
    return pkg
