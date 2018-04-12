{-# LANGUAGE OverloadedLists #-}
module Nirum.Targets.DocsSpec where

import Control.Monad
import GHC.Exts (IsList (toList))

import Data.Text hiding (empty)
import Data.Text.Encoding
import System.FilePath ((</>))
import Test.Hspec.Meta
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.XML.HXT.Core hiding (when)

import Nirum.Constructs.Annotation (empty)
import Nirum.Constructs.DeclarationSet (DeclarationSet)
import Nirum.Constructs.Module (Module (..))
import Nirum.Constructs.TypeDeclaration (TypeDeclaration (Import))
import qualified Nirum.Docs as D
import Nirum.Package
import Nirum.Package.Metadata (Target (compilePackage, toByteString))
import qualified Nirum.Targets.Docs as DT
import Nirum.TestFixtures

spec :: Spec
spec = describe "Docs" $ do
    let decls = [ Import ["zzz"] "qqq" "qqq" empty
                ] :: DeclarationSet TypeDeclaration
        mod1 = Module decls Nothing
        mod2 = Module decls $ Just "module level docs...\nblahblah"
        mod3 = Module decls $ Just "# One Spoqa Trinity Studio\nblahblah"
    specify "makeFilePath" $
        DT.makeFilePath ["foo", "bar", "baz"] `shouldBe`
            "foo" </> "bar" </> "baz" </> "index.html"
    specify "makeUri" $
        DT.makeUri ["foo", "bar", "baz"] `shouldBe` "foo/bar/baz/index.html"
    specify "moduleTitle" $ do
        fmap renderHtml (DT.moduleTitle mod1) `shouldBe` Nothing
        fmap renderHtml (DT.moduleTitle mod2) `shouldBe` Nothing
        fmap renderHtml (DT.moduleTitle mod3) `shouldBe`
            Just "One Spoqa Trinity Studio"
    specify "blockToHtml" $ do
        let h = D.Paragraph [D.Strong ["Hi!"]]
        renderHtml (DT.blockToHtml h) `shouldBe` "<p><strong>Hi!</strong></p>"
    specify "<title>" $ do
        pkg <- fixturePackage
        let t = target pkg
        forM_ (toList $ compilePackage (pkg :: Package DT.Docs)) $ \ (f, r) ->
            when (".html" `isSuffixOf` pack f) $ do
                let Right html = r
                let doc = readString
                              [withParseHTML yes, withWarnings no]
                              (unpack . decodeUtf8 . toByteString t $ html)
                titles <- runX $ doc //> hasName "title" /> getText
                titles `shouldNotBe` []
                forM_ titles $ \ title ->
                    pack title `shouldSatisfy`
                        isSuffixOf "Fixtures for Nirum tests"
