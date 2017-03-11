{-# LANGUAGE OverloadedLists #-}
module Nirum.Targets.DocsSpec where

import System.FilePath ((</>))
import Test.Hspec.Meta

import qualified Nirum.Targets.Docs as D

spec :: Spec
spec = do
    specify "makeFilePath" $
        D.makeFilePath ["foo", "bar", "baz"] `shouldBe`
            "foo" </> "bar" </> "baz" </> "index.html"
    specify "makeUri" $
        D.makeUri ["foo", "bar", "baz"] `shouldBe`
            "./" ++ ("foo" </> "bar" </> "baz" </> "index.html")
