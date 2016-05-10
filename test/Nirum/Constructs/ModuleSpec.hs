{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Nirum.Constructs.ModuleSpec where

import Test.Hspec.Meta

import Nirum.Constructs (Construct(toCode))
import Nirum.Constructs.DeclarationSet (DeclarationSet)
import Nirum.Constructs.Module (Module(..))
import Nirum.Constructs.TypeDeclaration ( Type(..)
                                        , TypeDeclaration(TypeDeclaration)
                                        )

spec :: Spec
spec =
    describe "Module" $ do
        let decls = [ TypeDeclaration "path" (Alias "text") $ Just "path string"
                    , TypeDeclaration "offset" (BoxedType "float64") Nothing
                    ] :: DeclarationSet TypeDeclaration
            mod1 = Module decls Nothing
            mod2 = Module decls $ Just "module level docs...\nblahblah"
        specify "toCode" $ do
            toCode mod1 `shouldBe` "\n\
                                   \type path = text;\n\
                                   \# path string\n\
                                   \\n\
                                   \boxed offset (float64);\n"
            toCode mod2 `shouldBe` "# module level docs...\n\
                                   \# blahblah\n\
                                   \\n\
                                   \type path = text;\n\
                                   \# path string\n\
                                   \\n\
                                   \boxed offset (float64);\n"
