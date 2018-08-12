{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Nirum.Targets.PythonSpec where

import qualified Data.Map.Strict as M
import Data.Either
import System.FilePath ((</>))
import Test.Hspec.Meta

import Nirum.Constructs.Annotation hiding (null)
import Nirum.Constructs.Annotation.Internal
import Nirum.Constructs.Module (Module (Module))
import Nirum.Constructs.TypeDeclaration hiding (Text)
import qualified Nirum.Package.Metadata as M
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
                files = M.compilePackage pkg
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
                files = M.compilePackage pkg
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
                files = M.compilePackage pkg
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
                files' = M.compilePackage pkg'
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

    describe "@numeric-constraints" $ do
        it "fails if unsupported arguments are present" $ do
            let Right annots = fromList
                    [ Annotation
                          "numeric-constraints"
                          [("min", Integer 1), ("unsupported", Integer 2)]
                    ]
            compareErrorMessage annots

        it "fails if unsupported arguments type is given" $ do
            let Right annots = fromList
                    [ Annotation
                          "numeric-constraints"
                          [("min", Integer 1), ("max", Text "2")]
                    ]
            compareErrorMessage annots

        it "success" $ do
            let Right annots = fromList
                    [ Annotation
                          "numeric-constraints"
                          [("min", Integer 1), ("max", Integer 2)]
                    ]
            let Just result = getResult annots
            isRight result `shouldBe` True
      where
          compareErrorMessage annots = do
              let Just result = getResult annots
              let Left errorMessage = result
              errorMessage `shouldBe`
                 "Unsupported arguments on @numeric-constraints"

          getResult annots =
              let
                  unboxed = TypeDeclaration "foo" (UnboxedType "int32") annots
                  (Source pkg _) = makeDummySource $ Module [unboxed] Nothing
                  files = M.compilePackage pkg
              in
                  M.lookup ("src" </> "foo" </> "__init__.py") files
