{-# LANGUAGE OverloadedLists, QuasiQuotes #-}
module Nirum.Constructs.ModuleSpec where

import Test.Hspec.Meta
import Text.InterpolatedString.Perl6 (q)

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Annotation as A (docs, empty, singleton)
import Nirum.Constructs.DeclarationSet (DeclarationSet)
import Nirum.Constructs.Module (Module (..), imports)
import Nirum.Constructs.TypeDeclaration ( Type (..)
                                        , TypeDeclaration ( Import
                                                          , TypeDeclaration
                                                          )
                                        )

spec :: Spec
spec =
    describe "Module" $ do
        let docsAnno = A.docs "path string"
            pathT = TypeDeclaration "path" (Alias "text") (singleton docsAnno)
            offsetT =
                TypeDeclaration "offset" (UnboxedType "float64") empty
            decls = [ Import ["foo", "bar"] "baz" "baz" empty
                    , Import ["foo", "bar"] "qux" "qux" empty
                    , Import ["zzz"] "qqq" "qqq" empty
                    , Import ["zzz"] "ppp" "ppp" empty
                    , Import ["xyz"] "asdf" "asdf" empty
                    , pathT
                    , offsetT
                    ] :: DeclarationSet TypeDeclaration
            decls2 = [ Import ["foo", "bar"] "qux" "baz" empty
                     ] :: DeclarationSet TypeDeclaration
            mod1 = Module decls Nothing
            mod2 = Module decls $ Just "module level docs...\nblahblah"
            mod3 = Module decls2 Nothing
        specify "imports" $ do
            imports mod1 `shouldBe` [ ( ["foo", "bar"]
                                      , [("baz", "baz"), ("qux", "qux")]
                                      )
                                    , (["xyz"], [("asdf", "asdf")])
                                    , ( ["zzz"]
                                      , [("qqq", "qqq"), ("ppp", "ppp")]
                                      )
                                    ]
            imports mod2 `shouldBe` imports mod1
        specify "toCode" $ do
            toCode mod1 `shouldBe` [q|import foo.bar (baz, qux);
import xyz (asdf);
import zzz (ppp, qqq);

type path = text;
# path string

unboxed offset (float64);
|]
            toCode mod2 `shouldBe` [q|# module level docs...
# blahblah
import foo.bar (baz, qux);
import xyz (asdf);
import zzz (ppp, qqq);

type path = text;
# path string

unboxed offset (float64);
|]
            toCode mod3 `shouldBe` [q|import foo.bar (baz as qux);


|]
