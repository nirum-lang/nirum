{-# LANGUAGE OverloadedLists #-}
module Nirum.Targets.DocsSpec where

import System.FilePath ((</>))
import Test.Hspec.Meta

import qualified Nirum.Targets.Docs as D

spec :: Spec
spec =
    specify "makeFilePath" $
        D.makeFilePath ["foo", "bar", "baz"] `shouldBe`
            "foo" </> "bar" </> "baz" </> "index.html"
