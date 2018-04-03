{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Nirum.TypeInstance.BoundModuleSpec where

import Data.Proxy

import Test.Hspec.Meta
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs.Annotation hiding (docs)
import Nirum.Constructs.Declaration
import Nirum.Constructs.Module hiding (docs)
import Nirum.Constructs.TypeDeclaration hiding (modulePath)
import Nirum.Package.Metadata
import Nirum.Package.MetadataSpec
import Nirum.PackageSpec (createValidPackage)
import Nirum.Targets.Python (Python (Python))
import Nirum.Targets.Python.CodeGen (minimumRuntime)
import Nirum.TypeInstance.BoundModule

spec :: Spec
spec = do
    testPackage (Python "nirum-examples" minimumRuntime [])
    testPackage DummyTarget

testPackage :: forall t . Target t => t -> Spec
testPackage target' = do
    let targetName' = targetName (Proxy :: Proxy t)
        validPackage = createValidPackage target'
    specify "resolveBoundModule" $ do
        let Just bm = resolveBoundModule ["foo"] validPackage
        boundPackage bm `shouldBe` validPackage
        modulePath bm `shouldBe` ["foo"]
        resolveBoundModule ["baz"] validPackage `shouldBe` Nothing
    describe [qq|BoundModule (target: $targetName')|] $ do
        let Just bm = resolveBoundModule ["foo", "bar"] validPackage
            Just abc = resolveBoundModule ["abc"] validPackage
            Just xyz = resolveBoundModule ["xyz"] validPackage
        specify "docs" $ do
            docs bm `shouldBe` Just "foo.bar"
            let Just bm' = resolveBoundModule ["foo"] validPackage
            docs bm' `shouldBe` Just "foo"
        specify "boundTypes" $ do
            boundTypes bm `shouldBe` []
            boundTypes abc `shouldBe` [TypeDeclaration "a" (Alias "text") empty]
            boundTypes xyz `shouldBe`
                [ Import ["abc"] (ImportName "a" Nothing) empty
                , TypeDeclaration "x" (Alias "text") empty
                ]
        specify "lookupType" $ do
            lookupType "a" bm `shouldBe` Missing
            lookupType "a" abc `shouldBe` Local (Alias "text")
            lookupType "a" xyz `shouldBe` Imported ["abc"] "a" (Alias "text")
            lookupType "x" bm `shouldBe` Missing
            lookupType "x" abc `shouldBe` Missing
            lookupType "x" xyz `shouldBe` Local (Alias "text")
            lookupType "text" bm `shouldBe`
                Imported coreModulePath "text" (PrimitiveType Text String)
            lookupType "text" abc `shouldBe` lookupType "text" bm
            lookupType "text" xyz `shouldBe` lookupType "text" bm
