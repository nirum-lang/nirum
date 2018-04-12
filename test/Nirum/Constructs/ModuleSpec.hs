{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Nirum.Constructs.ModuleSpec where

import Test.Hspec.Meta
import Text.InterpolatedString.Perl6 (q)

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Annotation as A (docs, empty, singleton)
import Nirum.Constructs.DeclarationSet (DeclarationSet)
import Nirum.Constructs.Identifier
import Nirum.Constructs.Module (Module (..), imports)
import Nirum.Constructs.TypeDeclaration as TD
    ( Type (..)
    , TypeDeclaration (Import, TypeDeclaration)
    , ImportName ( ImportName )
    )

ine :: Identifier -> TD.ImportName
ine n = TD.ImportName n Nothing


spec :: Spec
spec =
    describe "Module" $ do
        let docsAnno = A.docs "path string"
            pathT = TypeDeclaration "path" (Alias "text") (singleton docsAnno)
            offsetT =
                TypeDeclaration "offset" (UnboxedType "float64") empty
            decls = [ Import ["foo", "bar"] (ine "baz") empty
                    , Import ["foo", "bar"] (ine "qux") empty
                    , Import ["zzz"] (ine "qqq") empty
                    , Import ["zzz"] (ine "ppp") empty
                    , Import ["xyz"] (ine "asdf") empty
                    , Import
                          ["some"]
                          (TD.ImportName "over" $ Just "rainbow")
                          empty
                    , pathT
                    , offsetT
                    ] :: DeclarationSet TypeDeclaration
            mod1 = Module decls Nothing
            mod2 = Module decls $ Just "module level docs...\nblahblah"
        specify "imports" $ do
            imports mod1 `shouldBe` [ (["foo", "bar"], [ine "baz", ine "qux"])
                                    , (["xyz"], [ine "asdf"])
                                    , (["zzz"], [ine "qqq", ine "ppp"])
                                    , ( ["some"]
                                      , [TD.ImportName "over" $ Just "rainbow"]
                                      )
                                    ]
            imports mod2 `shouldBe` imports mod1
        specify "toCode" $ do
            toCode mod1 `shouldBe` [q|import foo.bar (baz, qux);
import some (over as rainbow);
import xyz (asdf);
import zzz (ppp, qqq);

type path = text;
# path string

unboxed offset (float64);
|]
            toCode mod2 `shouldBe` [q|# module level docs...
# blahblah
import foo.bar (baz, qux);
import some (over as rainbow);
import xyz (asdf);
import zzz (ppp, qqq);

type path = text;
# path string

unboxed offset (float64);
|]
