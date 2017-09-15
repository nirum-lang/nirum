module Nirum.Package.Metadata.WriterSpec where

import qualified Data.HashMap.Lazy as HM
import Test.Hspec.Meta
import qualified Text.Toml.Types as TM

import qualified Nirum.Package.Metadata.Writer as W

spec :: Spec
spec =
    describe "renderTable" $ do
        it "render string value" $ do
            let s = W.renderTable $ HM.fromList [("foo", TM.VString "bar")]
            s `shouldBe` "foo = \"bar\""
        it "render multiple string value" $ do
            let s = W.renderTable $ HM.fromList [ ("foo", TM.VString "bar")
                                                , ("baz", TM.VString "boo")
                                                ]
            s `shouldBe` "foo = \"bar\"\nbaz = \"boo\""
        it "render table" $ do
            let t = TM.VTable $ HM.fromList [("bar", TM.VString "baz")]
                s = W.renderTable $ HM.fromList [("foo", t)]
            s `shouldBe` "\n[foo]\nbar = \"baz\""
        it "render multiple table" $ do
            let fooTable = TM.VTable $ HM.fromList [("bar", TM.VString "baz")]
                loremTable = TM.VTable $
                    HM.fromList [("ipsum", TM.VString "que")]
                s = W.renderTable $ HM.fromList [ ("foo", fooTable)
                                                , ("lorem", loremTable)
                                                ]
            s `shouldBe` "\n[foo]\nbar = \"baz\"\n\n[lorem]\nipsum = \"que\""
        it "render string first, table later" $ do
            let targetTable = TM.VTable $ HM.fromList [ ( "language"
                                                        , TM.VString "python"
                                                        )
                                                      ]
                s = W.renderTable $
                        HM.fromList [ ("target", targetTable)
                                    , ("version", TM.VString "0.0.1")
                                    ]
            s `shouldBe`
                "version = \"0.0.1\"\n\n[target]\nlanguage = \"python\""
