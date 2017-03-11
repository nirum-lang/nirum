{-# LANGUAGE OverloadedLists, PartialTypeSignatures #-}
module Nirum.Targets.JavaScriptSpec ( spec
                                    ) where

import qualified Data.Map.Strict as M
import qualified Data.SemVer as SV
import Text.Toml.Types (emptyTable)
import Test.Hspec.Meta

import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Module (Module (..))
import Nirum.Targets.JavaScript
import Nirum.Package.Metadata ( Metadata (..)
                              , MetadataError ( FieldError )
                              , Package (..)
                              , parseTarget )
import qualified Nirum.Package.ModuleSet as MS


emptyModule :: Module
emptyModule = Module { types = DS.empty, docs = Nothing }


spec :: Spec
spec = do
    let js = JavaScript { packageName = "dummy" }
    let Right modules' = MS.fromList [ (["fruits"], emptyModule)
                                     , (["imported-commons"], emptyModule)
                                     , (["transports", "truck"], emptyModule)
                                     , (["transports", "container"], emptyModule)
                                     ]
    let package = Package { metadata = Metadata { version = SV.version 0 0 1 [] []
                                                , authors = []
                                                , target = js
                                                }
                          , modules = modules'
                          }
    describe "compilePackage'" $
        it "should produce JavaScript files per corresponding module" $ do
            let m = compilePackage' package
            M.keysSet m `shouldBe` [ "package.json"
                                   , "src/fruits.js"
                                   , "src/imported_commons.js"
                                   , "src/transports/truck.js"
                                   , "src/transports/container.js"
                                   ]
    describe "parseTarget" $
        it "should require \"name\" field" $
            (parseTarget emptyTable :: Either MetadataError JavaScript) `shouldBe` Left (FieldError "name")
