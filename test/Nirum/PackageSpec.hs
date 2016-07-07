{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Nirum.PackageSpec where

import qualified Data.Map.Strict as M
import System.FilePath ((</>))
import Test.Hspec.Meta

import Nirum.Constructs.Module (Module(Module))
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Constructs.TypeDeclaration ( Type(Alias)
                                        , TypeDeclaration ( Import
                                                          , TypeDeclaration
                                                          )
                                        )
import Nirum.Package ( ImportError ( CircularImportError
                                   , MissingImportError
                                   , MissingModulePathError
                                   )
                     , Package
                     , makePackage
                     , resolveModule
                     , scanModules
                     , scanPackage
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
            Right package <- scanPackage "."
            let path = "." </> "examples"
            Right builtinsM <- parseFile (path </> "builtins.nrm")
            Right productM <- parseFile (path </> "product.nrm")
            Right shapesM <- parseFile (path </> "shapes.nrm")
            Right countriesM <- parseFile (path </> "countries.nrm")
            Right addressM <- parseFile (path </> "address.nrm")
            let modules = [ (["examples", "builtins"], builtinsM)
                          , (["examples", "product"], productM)
                          , (["examples", "shapes"], shapesM)
                          , (["examples", "countries"], countriesM)
                          , (["examples", "address"], addressM)
                          ] :: M.Map ModulePath Module
            package `shouldBe` createPackage modules
        specify "scanModules" $ do
            let path = "." </> "examples"
            mods <- scanModules "."
            mods `shouldBe`
                [ (["examples", "builtins"], path </> "builtins.nrm")
                , (["examples", "product"], path </> "product.nrm")
                , (["examples", "shapes"], path </> "shapes.nrm")
                , (["examples", "countries"], path </> "countries.nrm")
                , (["examples", "address"], path </> "address.nrm")
                ]
            mods' <- scanModules path
            mods' `shouldBe` [ (["builtins"], path </> "builtins.nrm")
                             , (["product"], path </> "product.nrm")
                             , (["shapes"], path </> "shapes.nrm")
                             , (["countries"], path </> "countries.nrm")
                             , (["address"], path </> "address.nrm")
                             ]
