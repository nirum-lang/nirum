{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Nirum.PackageSpec where

import qualified Data.Map.Strict as M
import System.FilePath ((</>))
import Test.Hspec.Meta

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
                     , Package
                     , PackageError (ImportError)
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

createPackage :: M.Map ModulePath Module -> Package
createPackage modules' =
    case makePackage modules' of
        Right pkg -> pkg
        Left e -> error $ "errored: " ++ show e

validPackage :: Package
validPackage =
    createPackage [ (["foo", "bar"], Module [] $ Just "foo.bar")
                  , (["foo", "baz"], Module [] $ Just "foo.baz")
                  , (["foo"],        Module [] $ Just "foo")
                  , (["qux"],        Module [] $ Just "qux")
                  , ( ["abc"]
                    , Module [TypeDeclaration "a" (Alias "text") Nothing]
                             Nothing
                    )
                  , ( ["xyz"]
                    , Module [ Import ["abc"] "a"
                             , TypeDeclaration "x" (Alias "text") Nothing
                             ] Nothing
                    )
                  ]

missingImportsModules :: M.Map ModulePath Module
missingImportsModules =
    [ (["foo"], Module [ Import ["foo", "bar"] "xyz" -- MissingModulePathError
                       , Import ["foo", "bar"] "zzz" -- MissingModulePathError
                       , Import ["baz"] "qux"
                       ] Nothing)
    , (["baz"], Module [ TypeDeclaration "qux" (Alias "text") Nothing ] Nothing)
    , (["qux"], Module [ Import ["foo"] "abc" -- MissingImportError
                       , Import ["foo"] "def" -- MissingImportError
                       ] Nothing)
    ]

circularImportsModules :: M.Map ModulePath Module
circularImportsModules =
    [ (["asdf"], Module [ Import ["asdf"] "foo"
                        , TypeDeclaration "bar" (Alias "text") Nothing
                        ] Nothing)
    , (["abc", "def"], Module [Import ["abc", "ghi"] "bar"
                              , TypeDeclaration "foo" (Alias "text") Nothing
                              ] Nothing)
    , (["abc", "ghi"], Module [Import ["abc", "xyz"] "baz"
                              , TypeDeclaration "bar" (Alias "text") Nothing
                              ] Nothing)
    , (["abc", "xyz"], Module [Import ["abc", "def"] "foo"
                              , TypeDeclaration "baz" (Alias "text") Nothing
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
            makePackage missingImportsModules `shouldBe`
                Left [ MissingModulePathError ["foo"] ["foo", "bar"]
                     , MissingImportError ["qux"] ["foo"] "abc"
                     , MissingImportError ["qux"] ["foo"] "def"
                     ]
        specify "detectCircularImports" $
            makePackage circularImportsModules `shouldBe`
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
        specify "scanPackage" $ do
            let path = "." </> "examples"
            Right package <- scanPackage path
            Right builtinsM <- parseFile (path </> "builtins.nrm")
            Right productM <- parseFile (path </> "product.nrm")
            Right shapesM <- parseFile (path </> "shapes.nrm")
            Right countriesM <- parseFile (path </> "countries.nrm")
            Right addressM <- parseFile (path </> "address.nrm")
            let modules = [ (["builtins"], builtinsM)
                          , (["product"], productM)
                          , (["shapes"], shapesM)
                          , (["countries"], countriesM)
                          , (["address"], addressM)
                          ] :: M.Map ModulePath Module
            package `shouldBe` createPackage modules
            Left error' <- scanPackage $ "." </> "test" </> "import_error"
            error' `shouldBe`
                ImportError [MissingModulePathError ["import_error"] ["foo"]]
        specify "scanModules" $ do
            let path = "." </> "examples"
            mods <- scanModules "."
            mods `shouldBe`
                [ (["examples", "builtins"], path </> "builtins.nrm")
                , (["examples", "product"], path </> "product.nrm")
                , (["examples", "shapes"], path </> "shapes.nrm")
                , (["examples", "countries"], path </> "countries.nrm")
                , (["examples", "address"], path </> "address.nrm")
                , ( ["test", "import_error", "import_error"]
                  , "." </> "test" </> "import_error" </> "import_error.nrm"
                  )
                ]
            mods' <- scanModules path
            mods' `shouldBe` [ (["builtins"], path </> "builtins.nrm")
                             , (["product"], path </> "product.nrm")
                             , (["shapes"], path </> "shapes.nrm")
                             , (["countries"], path </> "countries.nrm")
                             , (["address"], path </> "address.nrm")
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
            types abc `shouldBe` [TypeDeclaration "a" (Alias "text") Nothing]
            types xyz `shouldBe` [ Import ["abc"] "a"
                                 , TypeDeclaration "x" (Alias "text") Nothing
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
