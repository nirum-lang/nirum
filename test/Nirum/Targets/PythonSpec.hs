{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Nirum.Targets.PythonSpec where

import qualified Data.Map.Strict as M
import System.FilePath ((</>))
import Test.Hspec.Meta

import Nirum.Constructs.Module (Module (Module))
import Nirum.Constructs.Name (Name (Name))
import Nirum.Package.Metadata (Target (compilePackage))
import Nirum.Targets.Python
    ( Source (Source)
    , parseModulePath
    , toNamePair
    )
import Nirum.Targets.Python.CodeGenSpec hiding (spec)

spec :: Spec
spec = do
    describe "toNamePair" $ do
        it "transforms the name to a Python code string of facial/behind pair" $
            do toNamePair "text" `shouldBe` "('text', 'text')"
               toNamePair (Name "test" "hello") `shouldBe` "('test', 'hello')"
        it "replaces hyphens to underscores" $ do
            toNamePair "hello-world" `shouldBe` "('hello_world', 'hello_world')"
            toNamePair (Name "hello-world" "annyeong-sesang") `shouldBe`
                "('hello_world', 'annyeong_sesang')"
        it "appends an underscore if the facial name is a Python keyword" $ do
            toNamePair "def" `shouldBe` "('def_', 'def')"
            toNamePair "lambda" `shouldBe` "('lambda_', 'lambda')"
            toNamePair (Name "abc" "lambda") `shouldBe` "('abc', 'lambda')"
            toNamePair (Name "lambda" "abc") `shouldBe` "('lambda_', 'abc')"

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
