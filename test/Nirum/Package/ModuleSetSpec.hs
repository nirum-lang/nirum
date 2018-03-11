{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Nirum.Package.ModuleSetSpec where

import Data.List (sort, sortOn)
import Data.Maybe (isNothing)
import Prelude hiding (length, lookup, null)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Test.Hspec.Meta

import Nirum.Constructs.Annotation (empty)
import Nirum.Constructs.Identifier
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Constructs.Module (Module (Module))
import Nirum.Constructs.TypeDeclaration as TD
    ( Type (Alias)
    , TypeDeclaration ( Import
                      , TypeDeclaration
                      )
    , ImportName ( ImportName )
    )
import Nirum.Package.ModuleSet ( ImportError ( CircularImportError
                                             , MissingImportError
                                             , MissingModulePathError
                                             )
                               , fromList
                               , fromMap
                               , keys
                               , keysSet
                               , length
                               , lookup
                               , null
                               , toAscList
                               , toList
                               , toMap
                               )

ine :: Identifier -> ImportName
ine n = ImportName n Nothing

fooBarModule :: Module
fooBarModule = Module [] $ Just "foo.bar"

validModules :: [(ModulePath, Module)]
validModules =
    [ ( ["abc"]
      , Module [TypeDeclaration "a" (Alias "text") empty]
               Nothing
      )
    , (["foo"], Module [] $ Just "foo")
    , (["foo", "bar"], fooBarModule)
    , (["foo", "baz"], Module [] $ Just "foo.baz")
    , (["qux"], Module [] $ Just "qux")
    , ( ["xyz"]
      , Module [ Import ["abc"] (ine "a") empty
               , TypeDeclaration "x" (Alias "text") empty
               ] Nothing
      )
    ]

missingImportsModules :: [(ModulePath, Module)]
missingImportsModules =
    [ ( ["foo"]
      , Module [ Import ["foo", "bar"] (ine "xyz") empty -- MissingModulePathError
               , Import ["foo", "bar"] (ine "zzz") empty -- MissingModulePathError
               , Import ["baz"] (ine "qux") empty
               ] Nothing
      )
    , ( ["baz"]
      , Module [ TypeDeclaration "qux" (Alias "text") empty ] Nothing
      )
    , (["qux"], Module [ Import ["foo"] (ine "abc") empty -- MissingImportError
                       , Import ["foo"] (ine "def") empty -- MissingImportError
                       ] Nothing)
    ]

circularImportsModules :: [(ModulePath, Module)]
circularImportsModules =
    [ (["asdf"], Module [ Import ["asdf"] (ine "foo") empty
                        , TypeDeclaration "bar" (Alias "text") empty
                        ] Nothing)
    , (["abc", "def"], Module [ Import ["abc", "ghi"] (ine "bar") empty
                              , TypeDeclaration
                                    "foo" (Alias "text") empty
                              ] Nothing)
    , (["abc", "ghi"], Module [ Import ["abc", "xyz"] (ine "baz") empty
                              , TypeDeclaration
                                    "bar" (Alias "text") empty
                              ] Nothing)
    , (["abc", "xyz"], Module [ Import ["abc", "def"] (ine "foo") empty
                              , TypeDeclaration
                                    "baz" (Alias "text") empty
                              ] Nothing)
    ]

spec :: Spec
spec =
    describe "ModuleSet" $ do
        context "(Map ModulePath Module)" $
            testImportErrors (fromMap . M.fromList)
        context "(Map ModulePath Module)" $
            testImportErrors fromList
        specify "fromMap" $ do
            let Right moduleSet = fromMap $ M.fromList validModules
            toAscList moduleSet `shouldBe` validModules
        specify "fromList" $ do
            let Right moduleSet = fromList validModules
            toAscList moduleSet `shouldBe` validModules
        let Right validModuleSet = fromList validModules
        specify "toMap" $
            toMap validModuleSet `shouldBe` M.fromList validModules
        specify "toList" $
            sortOn fst (toList validModuleSet) `shouldBe` validModules
        specify "toAscList" $
            toAscList validModuleSet `shouldBe` validModules
        specify "keys" $
            sort (keys validModuleSet) `shouldBe`
                sort [path | (path, _) <- validModules]
        specify "keysSet" $
            keysSet validModuleSet `shouldBe`
                S.fromList [p | (p, _) <- validModules]
        specify "lookup" $ do
            let Just mod' = lookup ["foo", "bar"] validModuleSet
            mod' `shouldBe` fooBarModule
            lookup ["wrong", "path"] validModuleSet `shouldSatisfy` isNothing
        specify "length" $
            length validModuleSet `shouldBe` 6
        specify "null" $
            validModuleSet `shouldNotSatisfy` null
  where
    testImportErrors makeModuleSet = do
        specify "detectMissingImports" $
            makeModuleSet missingImportsModules `shouldBe`
                Left [ MissingModulePathError ["foo"] ["foo", "bar"]
                     , MissingImportError ["qux"] ["foo"] "abc"
                     , MissingImportError ["qux"] ["foo"] "def"
                     ]
        specify "detectCircularImports" $
            makeModuleSet circularImportsModules `shouldBe`
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
