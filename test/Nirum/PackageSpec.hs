{-# LANGUAGE OverloadedLists, OverloadedStrings, QuasiQuotes,
             ScopedTypeVariables #-}
module Nirum.PackageSpec where

import Data.Either (isLeft, isRight)
import Data.Proxy (Proxy (Proxy))
import System.IO.Error (isDoesNotExistError)

import qualified Data.SemVer as SV
import System.FilePath ((</>))
import Test.Hspec.Meta
import Text.InterpolatedString.Perl6 (qq)
import qualified Text.Parsec.Error as PE
import Text.Parsec.Pos (sourceColumn, sourceLine)

import Nirum.Constructs.Annotation (empty)
import Nirum.Constructs.Module (Module (Module), coreModulePath)
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Constructs.TypeDeclaration ( JsonType (String)
                                        , PrimitiveTypeIdentifier (Text)
                                        , Type (Alias, PrimitiveType)
                                        , TypeDeclaration ( Import
                                                          , TypeDeclaration
                                                          )
                                        )
import Nirum.Package ( BoundModule (boundPackage, modulePath)
                     , Package (Package, metadata)
                     , PackageError (ImportError, MetadataError, ScanError)
                     , TypeLookup (Imported, Local, Missing)
                     , docs
                     , lookupType
                     , resolveBoundModule
                     , resolveModule
                     , scanModules
                     , scanPackage
                     , types
                     )
import Nirum.Package.Metadata ( Metadata (Metadata, authors, target, version)
                              , MetadataError (FormatError)
                              , Target (targetName)
                              )
import Nirum.Package.MetadataSpec (DummyTarget (DummyTarget))
import Nirum.Package.ModuleSet ( ImportError (MissingModulePathError)
                               , fromList
                               )
import Nirum.Package.ModuleSetSpec (validModules)
import Nirum.Parser (parseFile)
import Nirum.Targets.Python (Python (Python), minimumRuntime)

createPackage :: Metadata t -> [(ModulePath, Module)] -> Package t
createPackage metadata' modules' =
    case fromList modules' of
        Right ms -> Package metadata' ms
        Left e -> error $ "errored: " ++ show e

createValidPackage :: t -> Package t
createValidPackage t = createPackage Metadata { version = SV.initial
                                              , authors = []
                                              , target = t
                                              } validModules

spec :: Spec
spec = do
    testPackage (Python "nirum-examples" minimumRuntime)
    testPackage DummyTarget

testPackage :: forall t . Target t => t -> Spec
testPackage target' = do
    let targetName' = targetName (Proxy :: Proxy t)
        validPackage = createValidPackage target'
    describe [qq|Package (target: $targetName')|] $ do
        specify "resolveModule" $ do
            resolveModule ["foo"] validPackage `shouldBe`
                Just (Module [] $ Just "foo")
            resolveModule ["foo", "bar"] validPackage `shouldBe`
                Just (Module [] $ Just "foo.bar")
            resolveModule ["qux"] validPackage `shouldBe`
                Just (Module [] $ Just "qux")
            resolveModule ["baz"] validPackage `shouldBe` Nothing
        specify "resolveBoundModule" $ do
            let Just bm = resolveBoundModule ["foo"] validPackage
            boundPackage bm `shouldBe` validPackage
            modulePath bm `shouldBe` ["foo"]
            resolveBoundModule ["baz"] validPackage `shouldBe` Nothing
        describe "scanPackage" $ do
            it "returns Package value when all is well" $ do
                let path = "." </> "examples"
                package' <- scanPackage' path
                package' `shouldSatisfy` isRight
                let Right package = package'
                Right builtinsM <- parseFile (path </> "builtins.nrm")
                Right productM <- parseFile (path </> "product.nrm")
                Right shapesM <- parseFile (path </> "shapes.nrm")
                Right countriesM <- parseFile (path </> "countries.nrm")
                Right addressM <- parseFile (path </> "address.nrm")
                Right pdfServiceM <- parseFile (path </> "pdf-service.nrm")
                let modules = [ (["builtins"], builtinsM)
                              , (["product"], productM)
                              , (["shapes"], shapesM)
                              , (["countries"], countriesM)
                              , (["address"], addressM)
                              , (["pdf-service"], pdfServiceM)
                              ] :: [(ModulePath, Module)]
                    metadata' = Metadata { version = SV.version 0 3 0 [] []
                                         , authors = []
                                         , target = target'
                                         }
                metadata package `shouldBe` metadata'
                package `shouldBe` createPackage metadata' modules
            let testDir = "." </> "test"
            it "returns ScanError if the directory lacks package.toml" $ do
                scanResult <- scanPackage' $ testDir </> "scan_error"
                scanResult `shouldSatisfy` isLeft
                let Left (ScanError filePath ioError') = scanResult
                filePath `shouldBe` testDir </> "scan_error" </> "package.toml"
                ioError' `shouldSatisfy` isDoesNotExistError
            it "returns MetadataError if the package.toml is invalid" $ do
                scanResult <- scanPackage' $ testDir </> "metadata_error"
                scanResult `shouldSatisfy` isLeft
                let Left (MetadataError (FormatError e)) = scanResult
                sourceLine (PE.errorPos e) `shouldBe` 3
                sourceColumn (PE.errorPos e) `shouldBe` 14
            it "returns ImportError if a module imports an absent module" $ do
                scanResult <- scanPackage' $ testDir </> "import_error"
                scanResult `shouldSatisfy` isLeft
                let Left (ImportError l) = scanResult
                l `shouldBe` [MissingModulePathError ["import_error"] ["foo"]]
        specify "scanModules" $ do
            let path = "." </> "examples"
            mods' <- scanModules path
            mods' `shouldBe` [ (["builtins"], path </> "builtins.nrm")
                             , (["product"], path </> "product.nrm")
                             , (["shapes"], path </> "shapes.nrm")
                             , (["countries"], path </> "countries.nrm")
                             , (["address"], path </> "address.nrm")
                             , (["pdf-service"], path </> "pdf-service.nrm")
                             ]
    describe [qq|BoundModule (target: $targetName')|] $ do
        let Just bm = resolveBoundModule ["foo", "bar"] validPackage
            Just abc = resolveBoundModule ["abc"] validPackage
            Just xyz = resolveBoundModule ["xyz"] validPackage
        specify "docs" $ do
            docs bm `shouldBe` Just "foo.bar"
            let Just bm' = resolveBoundModule ["foo"] validPackage
            docs bm' `shouldBe` Just "foo"
        specify "types" $ do
            types bm `shouldBe` []
            types abc `shouldBe` [TypeDeclaration "a" (Alias "text") empty]
            types xyz `shouldBe` [ Import ["abc"] "a" empty
                                 , TypeDeclaration "x" (Alias "text") empty
                                 ]
        specify "lookupType" $ do
            lookupType "a" bm `shouldBe` Missing
            lookupType "a" abc `shouldBe` Local (Alias "text")
            lookupType "a" xyz `shouldBe` Imported ["abc"] (Alias "text")
            lookupType "x" bm `shouldBe` Missing
            lookupType "x" abc `shouldBe` Missing
            lookupType "x" xyz `shouldBe` Local (Alias "text")
            lookupType "text" bm `shouldBe`
                Imported coreModulePath (PrimitiveType Text String)
            lookupType "text" abc `shouldBe` lookupType "text" bm
            lookupType "text" xyz `shouldBe` lookupType "text" bm
  where
    scanPackage' :: FilePath -> IO (Either PackageError (Package t))
    scanPackage' = scanPackage
