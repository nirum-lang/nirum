{-# LANGUAGE OverloadedLists #-}
module Nirum.PackageSpec where

import Data.Either (isRight)
import System.IO.Error (isDoesNotExistError)

import qualified Data.Map.Strict as M
import Data.SemVer (Version, initial, version)
import System.FilePath ((</>))
import Test.Hspec.Meta
import qualified Text.Parsec.Error as PE
import Text.Parsec.Pos (sourceColumn, sourceLine)

import Nirum.Constructs.Annotation (empty)
import Nirum.Constructs.Module (Module(Module), coreModulePath)
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Constructs.TypeDeclaration ( JsonType(String)
                                        , PrimitiveTypeIdentifier(Text)
                                        , Type(Alias, PrimitiveType)
                                        , TypeDeclaration ( Import
                                                          , TypeDeclaration
                                                          )
                                        )
import Nirum.Package ( BoundModule(boundPackage, modulePath)
                     , ImportError ( CircularImportError
                                   , MissingImportError
                                   , MissingModulePathError
                                   )
                     , MetadataError ( FieldError
                                     , FieldTypeError
                                     , FieldValueError
                                     , FormatError
                                     )
                     , Package
                     , PackageError (ImportError, MetadataError, ScanError)
                     , TypeLookup(Imported, Local, Missing)
                     , docs
                     , lookupType
                     , makePackage
                     , resolveBoundModule
                     , resolveModule
                     , scanModules
                     , scanPackage
                     , types
                     )
import Nirum.Parser (parseFile)

createPackage' :: Version -> M.Map ModulePath Module -> Package
createPackage' ver modules' =
    case makePackage ver modules' of
        Right pkg -> pkg
        Left e -> error $ "errored: " ++ show e

createPackage :: M.Map ModulePath Module -> Package
createPackage = createPackage' initial

validPackage :: Package
validPackage =
    createPackage [ (["foo", "bar"], Module [] $ Just "foo.bar")
                  , (["foo", "baz"], Module [] $ Just "foo.baz")
                  , (["foo"],        Module [] $ Just "foo")
                  , (["qux"],        Module [] $ Just "qux")
                  , ( ["abc"]
                    , Module [TypeDeclaration "a" (Alias "text") empty]
                             Nothing
                    )
                  , ( ["xyz"]
                    , Module [ Import ["abc"] "a" empty
                             , TypeDeclaration "x" (Alias "text") empty
                             ] Nothing
                    )
                  ]

missingImportsModules :: M.Map ModulePath Module
missingImportsModules =
    [ ( ["foo"]
      , Module [ Import ["foo", "bar"] "xyz" empty -- MissingModulePathError
               , Import ["foo", "bar"] "zzz" empty -- MissingModulePathError
               , Import ["baz"] "qux" empty
               ] Nothing
      )
    , ( ["baz"]
      , Module [ TypeDeclaration "qux" (Alias "text") empty ] Nothing
      )
    , (["qux"], Module [ Import ["foo"] "abc" empty -- MissingImportError
                       , Import ["foo"] "def" empty -- MissingImportError
                       ] Nothing)
    ]

circularImportsModules :: M.Map ModulePath Module
circularImportsModules =
    [ (["asdf"], Module [ Import ["asdf"] "foo" empty
                        , TypeDeclaration "bar" (Alias "text") empty
                        ] Nothing)
    , (["abc", "def"], Module [ Import ["abc", "ghi"] "bar" empty
                              , TypeDeclaration
                                    "foo" (Alias "text") empty
                              ] Nothing)
    , (["abc", "ghi"], Module [ Import ["abc", "xyz"] "baz" empty
                              , TypeDeclaration
                                    "bar" (Alias "text") empty
                              ] Nothing)
    , (["abc", "xyz"], Module [ Import ["abc", "def"] "foo" empty
                              , TypeDeclaration
                                    "baz" (Alias "text") empty
                              ] Nothing)
    ]

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
        specify "detectMissingImports" $
            makePackage initial missingImportsModules `shouldBe`
                Left [ MissingModulePathError ["foo"] ["foo", "bar"]
                     , MissingImportError ["qux"] ["foo"] "abc"
                     , MissingImportError ["qux"] ["foo"] "def"
                     ]
        specify "detectCircularImports" $
            makePackage initial circularImportsModules `shouldBe`
                Left [ CircularImportError [["asdf"], ["asdf"]]
                     , MissingImportError ["asdf"] ["asdf"] "foo"
                     , CircularImportError [ ["abc", "def"]
                                           , ["abc", "ghi"]
                                           , ["abc", "xyz"]
                                           , ["abc", "def"]
                                           ]
                     , CircularImportError [ ["abc", "ghi"]
                                           , ["abc", "xyz"]
                                           , ["abc", "def"]
                                           , ["abc", "ghi"]
                                           ]
                     , CircularImportError [ ["abc", "xyz"]
                                           , ["abc", "def"]
                                           , ["abc", "ghi"]
                                           , ["abc", "xyz"]
                                           ]
                     ]
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
                              ] :: M.Map ModulePath Module
                package `shouldBe` createPackage' (version 0 2 0 [] []) modules
            let testDir = "." </> "test"
            it "returns ScanError if the directory lacks package.toml" $ do
                Left (ScanError filePath ioError') <-
                    scanPackage $ testDir </> "scan_error"
                filePath `shouldBe` testDir </> "scan_error" </> "package.toml"
                ioError' `shouldSatisfy` isDoesNotExistError
            it "returns MetadataError (FormatError) if the package.toml is \
               \not a valid TOML file" $ do
                Left (MetadataError (FormatError e)) <-
                    scanPackage $ testDir </> "metadata_format_error"
                sourceLine (PE.errorPos e) `shouldBe` 3
                sourceColumn (PE.errorPos e) `shouldBe` 14
            it "returns MetadataError (FieldError) if the package.toml lacks \
               \any required fields" $ do
                Left (MetadataError (FieldError field)) <-
                    scanPackage $ testDir </> "metadata_field_error"
                field `shouldBe` "version"
            it "returns MetadataError (FieldTypeError) if some fields of \
               \the package.toml has a value of unexpected type" $ do
                Left (MetadataError (FieldTypeError fName fExpected fActual))
                    <- scanPackage $ testDir </> "metadata_field_type_error"
                fName `shouldBe` "version"
                fExpected `shouldBe` "string"
                fActual `shouldBe` "integer (123)"
            it "returns MetadataError (FieldValueError) if some fields of \
               \the package.toml has an invalid/malformed value" $ do
                Left (MetadataError (FieldValueError fieldName msg))
                    <- scanPackage $ testDir </> "metadata_field_value_error"
                fieldName `shouldBe` "version"
                msg `shouldBe`
                    "expected a semver string (e.g. \"1.2.3\"), not \"0/2/0\""
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
