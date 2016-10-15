{-# LANGUAGE OverloadedLists #-}
module Nirum.PackageSpec where

import Data.Either (isRight)
import System.IO.Error (isDoesNotExistError)

import qualified Data.SemVer as SV
import System.FilePath ((</>))
import Test.Hspec.Meta
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
                     , Package (Package)
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
import Nirum.Package.Metadata ( Metadata (Metadata, authors, version)
                              , MetadataError (FormatError)
                              )
import Nirum.Package.ModuleSet ( ImportError (MissingModulePathError)
                               , fromList
                               )
import Nirum.Package.ModuleSetSpec (validModules)
import Nirum.Parser (parseFile)

createPackage :: Metadata -> [(ModulePath, Module)] -> Package
createPackage metadata' modules' =
    case fromList modules' of
        Right ms -> Package metadata' ms
        Left e -> error $ "errored: " ++ show e

validPackage :: Package
validPackage = createPackage Metadata { version = SV.initial
                                      , authors = []
                                      } validModules

spec :: Spec
spec = do
    describe "Package" $ do
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
                package' <- scanPackage path
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
                package `shouldBe`
                    createPackage Metadata { version = SV.version 0 3 0 [] []
                                           , authors = []
                                           } modules
            let testDir = "." </> "test"
            it "returns ScanError if the directory lacks package.toml" $ do
                Left (ScanError filePath ioError') <-
                    scanPackage $ testDir </> "scan_error"
                filePath `shouldBe` testDir </> "scan_error" </> "package.toml"
                ioError' `shouldSatisfy` isDoesNotExistError
            it "returns MetadataError if the package.toml is invalid" $ do
                Left (MetadataError (FormatError e)) <-
                    scanPackage $ testDir </> "metadata_error"
                sourceLine (PE.errorPos e) `shouldBe` 3
                sourceColumn (PE.errorPos e) `shouldBe` 14
            it "returns ImportError if a module imports an absent module" $ do
                Left (ImportError l) <- scanPackage $ testDir </> "import_error"
                l `shouldBe` [MissingModulePathError ["import_error"] ["foo"]]
        specify "scanModules" $ do
            let path = "." </> "examples"
            mods <- scanModules "."
            mods `shouldBe`
                [ (["examples", "builtins"], path </> "builtins.nrm")
                , (["examples", "product"], path </> "product.nrm")
                , (["examples", "shapes"], path </> "shapes.nrm")
                , (["examples", "countries"], path </> "countries.nrm")
                , (["examples", "address"], path </> "address.nrm")
                , (["examples", "pdf-service"], path </> "pdf-service.nrm")
                , ( ["test", "import_error", "import_error"]
                  , "." </> "test" </> "import_error" </> "import_error.nrm"
                  )
                , ( ["test", "scan_error", "scan_error"]
                  , "." </> "test" </> "scan_error" </> "scan_error.nrm"
                  )
                ]
            mods' <- scanModules path
            mods' `shouldBe` [ (["builtins"], path </> "builtins.nrm")
                             , (["product"], path </> "product.nrm")
                             , (["shapes"], path </> "shapes.nrm")
                             , (["countries"], path </> "countries.nrm")
                             , (["address"], path </> "address.nrm")
                             , (["pdf-service"], path </> "pdf-service.nrm")
                             ]
    describe "BoundModule" $ do
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
