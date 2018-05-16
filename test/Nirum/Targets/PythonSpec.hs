{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Nirum.Targets.PythonSpec where

import qualified Data.Map.Strict as M
import System.FilePath ((</>))
import Test.Hspec.Meta

import Nirum.Constructs.Module (Module (Module))
import Nirum.Package.Metadata (Target (compilePackage))
import Nirum.Targets.Python
    ( Source (Source)
    , parseModulePath
    )
import Nirum.Targets.Python.CodeGenSpec hiding (spec)

spec :: Spec
spec = do
    describe "compilePackage" $ do
        it "returns a Map of file paths and their contents to generate" $ do
            let (Source pkg _) = makeDummySource $ Module [] Nothing
                files = compilePackage pkg
                directoryStructure =
                    [ "src-py2" </> "foo" </> "__init__.py"
                    , "src-py2" </> "foo" </> "bar" </> "__init__.py"
                    , "src-py2" </> "qux" </> "__init__.py"
                    , "src" </> "foo" </> "__init__.py"
                    , "src" </> "foo" </> "bar" </> "__init__.py"
                    , "src" </> "qux" </> "__init__.py"
                    , "setup.py"
                    , "MANIFEST.in"
                    ]
            M.keysSet files `shouldBe` directoryStructure
        it "creates an emtpy Python package directory if necessary" $ do
            let (Source pkg _) = makeDummySource' ["test"] (Module [] Nothing)
                                                  []
                files = compilePackage pkg
                directoryStructure =
                    [ "src-py2" </> "test" </> "__init__.py"
                    , "src-py2" </> "test" </> "foo" </> "__init__.py"
                    , "src-py2" </> "test" </> "foo" </> "bar" </> "__init__.py"
                    , "src-py2" </> "test" </> "qux" </> "__init__.py"
                    , "src" </> "test" </> "__init__.py"
                    , "src" </> "test" </> "foo" </> "__init__.py"
                    , "src" </> "test" </> "foo" </> "bar" </> "__init__.py"
                    , "src" </> "test" </> "qux" </> "__init__.py"
                    , "setup.py"
                    , "MANIFEST.in"
                    ]
            M.keysSet files `shouldBe` directoryStructure
        it "generates renamed package dirs if renames are configured" $ do
            let (Source pkg _) = makeDummySource' [] (Module [] Nothing)
                                                  [(["foo"], ["quz"])]
                files = compilePackage pkg
                directoryStructure =
                    [ "src-py2" </> "quz" </> "__init__.py"
                    , "src-py2" </> "quz" </> "bar" </> "__init__.py"
                    , "src-py2" </> "qux" </> "__init__.py"
                    , "src" </> "quz" </> "__init__.py"
                    , "src" </> "quz" </> "bar" </> "__init__.py"
                    , "src" </> "qux" </> "__init__.py"
                    , "setup.py"
                    , "MANIFEST.in"
                    ]
            M.keysSet files `shouldBe` directoryStructure
            let (Source pkg' _) = makeDummySource' [] (Module [] Nothing)
                                                   [(["foo", "bar"], ["bar"])]
                files' = compilePackage pkg'
                directoryStructure' =
                    [ "src-py2" </> "foo" </> "__init__.py"
                    , "src-py2" </> "bar" </> "__init__.py"
                    , "src-py2" </> "qux" </> "__init__.py"
                    , "src" </> "foo" </> "__init__.py"
                    , "src" </> "bar" </> "__init__.py"
                    , "src" </> "qux" </> "__init__.py"
                    , "setup.py"
                    , "MANIFEST.in"
                    ]
            M.keysSet files' `shouldBe` directoryStructure'

    specify "parseModulePath" $ do
        parseModulePath "" `shouldBe` Nothing
        parseModulePath "foo" `shouldBe` Just ["foo"]
        parseModulePath "foo.bar" `shouldBe` Just ["foo", "bar"]
        parseModulePath "foo.bar-baz" `shouldBe` Just ["foo", "bar-baz"]
        parseModulePath "foo." `shouldBe` Nothing
        parseModulePath "foo.bar." `shouldBe` Nothing
        parseModulePath ".foo" `shouldBe` Nothing
        parseModulePath ".foo.bar" `shouldBe` Nothing
        parseModulePath "foo..bar" `shouldBe` Nothing
        parseModulePath "foo.bar>" `shouldBe` Nothing
        parseModulePath "foo.bar-" `shouldBe` Nothing
