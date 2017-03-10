{-# LANGUAGE PartialTypeSignatures #-}
module Nirum.Targets.JavaScriptSpec ( spec ) where

import Text.Toml.Types ( emptyTable )
import Test.Hspec.Meta

import Nirum.Targets.JavaScript
import Nirum.Package.Metadata ( MetadataError ( FieldError )
                              , parseTarget )


spec :: Spec
spec =
    describe "parseTarget" $
        it "should require \"name\" field" $
            (parseTarget emptyTable :: Either MetadataError JavaScript) `shouldBe` Left (FieldError "name")
