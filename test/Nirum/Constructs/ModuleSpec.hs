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
        let pathT = TypeDeclaration "path" (Alias "text") $ Just "path string"
            offsetT = TypeDeclaration "offset" (BoxedType "float64") Nothing
            decls = [pathT, offsetT] :: DeclarationSet TypeDeclaration
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
