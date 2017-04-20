{-# LANGUAGE OverloadedLists #-}
module Nirum.Targets.DocsSpec where

import System.FilePath ((</>))
import Test.Hspec.Meta
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Nirum.Constructs.Annotation (empty)
import Nirum.Constructs.DeclarationSet (DeclarationSet)
import Nirum.Constructs.Module (Module (..))
import Nirum.Constructs.TypeDeclaration (TypeDeclaration (Import))
import qualified Nirum.Targets.Docs as D

spec :: Spec
spec = describe "Docs" $ do
    let decls = [Import ["zzz"] "qqq" empty] :: DeclarationSet TypeDeclaration
        mod1 = Module decls Nothing
        mod2 = Module decls $ Just "module level docs...\nblahblah"
        mod3 = Module decls $ Just "# One Spoqa Trinity Studio\nblahblah"
    specify "makeFilePath" $
        D.makeFilePath ["foo", "bar", "baz"] `shouldBe`
            "foo" </> "bar" </> "baz" </> "index.html"
    specify "makeUri" $
        D.makeUri ["foo", "bar", "baz"] `shouldBe` "foo/bar/baz/index.html"
    specify "moduleTitle" $ do
        fmap renderHtml (D.moduleTitle mod1) `shouldBe` Nothing
        fmap renderHtml (D.moduleTitle mod2) `shouldBe` Nothing
        fmap renderHtml (D.moduleTitle mod3) `shouldBe`
            Just "One Spoqa Trinity Studio"
