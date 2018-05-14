{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.Python.CodeGenSpec
    ( makeDummySource
    , makeDummySource'
    , pythonVersionSpecs
    , spec
    ) where

import Control.Monad
import Data.Either
import Data.Maybe

import Data.SemVer hiding (Identifier, metadata)
import Test.Hspec.Meta
import Text.Email.Validate (emailAddress)
import Text.InterpolatedString.Perl6 (q, qq)

import Nirum.Constructs.Annotation (empty)
import Nirum.Constructs.Identifier
import Nirum.Constructs.Module
import Nirum.Constructs.ModulePath
import Nirum.Constructs.TypeDeclaration
import Nirum.Package.Metadata
import Nirum.Package.ModuleSet
import Nirum.PackageSpec (createPackage)
import Nirum.Targets.Python (Source (..))
import Nirum.Targets.Python.CodeGen hiding (empty, renames)
import qualified Nirum.Targets.Python.CodeGen as CodeGen
import Nirum.TypeInstance.BoundModule

makeDummySource' :: [Identifier] -> Module -> RenameMap -> Source
makeDummySource' pathPrefix m renames =
    Source pkg $ fromJust $ resolveBoundModule ["foo"] pkg
  where
    mp :: [Identifier] -> ModulePath
    mp identifiers = fromJust $ fromIdentifiers (pathPrefix ++ identifiers)
    metadata' :: Metadata Python
    metadata' = Metadata
        { Nirum.Package.Metadata.version = Data.SemVer.version 1 2 3 [] []
        , authors =
              [ Author
                    { name = "John Doe"
                    , email = Just (fromJust $ emailAddress "john@example.com")
                    , uri = Nothing
                    }
              ]
        , description = Just "Package description"
        , license = Just "MIT"
        , Nirum.Package.Metadata.keywords = ["sample", "example", "nirum"]
        , target = Python "sample-package" minimumRuntime renames ["classifier"]
        }
    pkg :: Package Python
    pkg = createPackage
            metadata'
            [ (mp ["foo"], m)
            , ( mp ["foo", "bar"]
              , Module [ Import (mp ["qux"]) "path" "path" empty
                       , TypeDeclaration "path-unbox" (UnboxedType "path") empty
                       , TypeDeclaration "int-unbox"
                                         (UnboxedType "bigint") empty
                       , TypeDeclaration "point"
                                         (RecordType [ Field "x" "int64" empty
                                                     , Field "y" "int64" empty
                                                     ])
                                         empty
                       ] Nothing
              )
            , ( mp ["qux"]
              , Module
                  [ TypeDeclaration "path" (Alias "text") empty
                  , TypeDeclaration "name" (UnboxedType "text") empty
                  ]
                  Nothing
              )
            ]

makeDummySource :: Module -> Source
makeDummySource m = makeDummySource' [] m []

pythonVersionSpecs :: (PythonVersion -> Spec) -> Spec
pythonVersionSpecs =
    parallel . forM_ ([Python2, Python3] :: [PythonVersion])

spec' :: Spec
spec' = pythonVersionSpecs $ \ ver -> do
    let empty' = CodeGen.empty ver
        -- compileError :: CodeGen a -> Maybe CompileError
        compileError cg = either Just (const Nothing) $ fst $
            runCodeGen cg empty'

    describe [qq|CodeGen ($ver)|] $ do
        specify "packages and imports" $ do
            let c = do
                    insertStandardImport "sys"
                    insertThirdPartyImports
                        [("nirum", ["serialize_unboxed_type"])]
                    insertLocalImport ".." "Gender"
                    insertStandardImport "os"
                    insertThirdPartyImports [("nirum", ["serialize_enum_type"])]
                    insertThirdPartyImportsA
                        [("nirum.constructs", [("name_dict_type", "NameDict")])]
                    insertLocalImport ".." "Path"
            let (e, ctx) = runCodeGen c empty'
            e `shouldSatisfy` isRight
            standardImports ctx `shouldBe` [("os", "os"), ("sys", "sys")]
            thirdPartyImports ctx `shouldBe`
                [ ( "nirum"
                  , [ ("serialize_unboxed_type", "serialize_unboxed_type")
                    , ("serialize_enum_type", "serialize_enum_type")
                    ]
                  )
                , ( "nirum.constructs"
                  , [("name_dict_type", "NameDict")]
                  )
                ]
            localImports ctx `shouldBe` [("..", ["Gender", "Path"])]
            localImportsMap ctx `shouldBe`
                [ ( ".."
                  , [ ("Gender", "Gender")
                    , ("Path", "Path")
                    ]
                  )
                ]
        specify "importStandardLibrary" $ do
            let codeGen1 = importStandardLibrary "io"
            let (e1, ctx1) = runCodeGen codeGen1 empty'
            e1 `shouldBe` Right "_io"
            standardImports ctx1 `shouldBe` [("_io", "io")]
            standardImportSet ctx1 `shouldBe` ["io"]
            thirdPartyImports ctx1 `shouldBe` []
            localImports ctx1 `shouldBe` []
            compileError codeGen1 `shouldBe` Nothing
            -- importing a nested module, i.e., import path that contains "."
            let codeGen2 = codeGen1 >> importStandardLibrary "os.path"
            let (e2, ctx2) = runCodeGen codeGen2 empty'
            e2 `shouldBe` Right "_os_path"
            standardImports ctx2 `shouldBe`
                [("_os_path", "os.path"), ("_io", "io")]
            standardImportSet ctx2 `shouldBe` ["os.path", "io"]
            thirdPartyImports ctx2 `shouldBe` []
            localImports ctx2 `shouldBe` []
            compileError codeGen2 `shouldBe` Nothing
            -- importing a module that begins with "_"
            let codeGen3 = codeGen2 >> importStandardLibrary "__builtin__"
            let (e3, ctx3) = runCodeGen codeGen3 empty'
            e3 `shouldBe` Right "__builtin__"
            standardImports ctx3 `shouldBe`
                [ ("__builtin__", "__builtin__")
                , ("_os_path", "os.path")
                , ("_io", "io")
                ]
            standardImportSet ctx3 `shouldBe` ["__builtin__", "os.path", "io"]
            thirdPartyImports ctx3 `shouldBe` []
            localImports ctx3 `shouldBe` []
            compileError codeGen3 `shouldBe` Nothing
        specify "importBuiltins" $ do
            let codeGen1 = importBuiltins
            let (e1, ctx1) = runCodeGen codeGen1 empty'
            e1 `shouldSatisfy` isRight
            standardImports ctx1 `shouldBe`
                [ if ver == Python2
                  then ("__builtin__", "__builtin__")
                  else ("__builtin__", "builtins")
                ]
            standardImportSet ctx1 `shouldBe`
                [if ver == Python2 then "__builtin__" else "builtins"]
            thirdPartyImports ctx1 `shouldBe` []
            localImports ctx1 `shouldBe` []
            compileError codeGen1 `shouldBe` Nothing
        specify "insertStandardImport" $ do
            let codeGen1 = insertStandardImport "sys"
            let (e1, ctx1) = runCodeGen codeGen1 empty'
            e1 `shouldSatisfy` isRight
            standardImports ctx1 `shouldBe` [("sys", "sys")]
            standardImportSet ctx1 `shouldBe` ["sys"]
            thirdPartyImports ctx1 `shouldBe` []
            localImports ctx1 `shouldBe` []
            compileError codeGen1 `shouldBe` Nothing
            let codeGen2 = codeGen1 >> insertStandardImport "os"
            let (e2, ctx2) = runCodeGen codeGen2 empty'
            e2 `shouldSatisfy` isRight
            standardImports ctx2 `shouldBe` [("os", "os"), ("sys", "sys")]
            standardImportSet ctx2 `shouldBe` ["os", "sys"]
            thirdPartyImports ctx2 `shouldBe` []
            localImports ctx2 `shouldBe` []
            compileError codeGen2 `shouldBe` Nothing
        specify "insertStandardImportA" $ do
            let codeGen1 = insertStandardImportA "_csv" "csv"
            let (e1, ctx1) = runCodeGen codeGen1 empty'
            e1 `shouldSatisfy` isRight
            standardImports ctx1 `shouldBe` [("_csv", "csv")]
            standardImportSet ctx1 `shouldBe` ["csv"]
            thirdPartyImports ctx1 `shouldBe` []
            localImports ctx1 `shouldBe` []
            compileError codeGen1 `shouldBe` Nothing
            let codeGen2 = codeGen1 >> insertStandardImportA "_gc" "gc"
            let (e2, ctx2) = runCodeGen codeGen2 empty'
            e2 `shouldSatisfy` isRight
            standardImports ctx2 `shouldBe` [("_gc", "gc"), ("_csv", "csv")]
            standardImportSet ctx2 `shouldBe` ["gc", "csv"]
            thirdPartyImports ctx2 `shouldBe` []
            localImports ctx2 `shouldBe` []
            compileError codeGen2 `shouldBe` Nothing
        specify "collectionsAbc" $ do
            let expected@(expectedModule, _) = case ver of
                    Python2 -> ("_collections", "collections")
                    Python3 -> ("_collections_abc", "collections.abc")
            let (abc, ctx) = runCodeGen collectionsAbc empty'
            abc `shouldBe` Right expectedModule
            standardImports ctx `shouldBe` [expected]

        let evalContext f = snd $ runCodeGen f empty'
        let req = empty'
        let req2 = req { dependencies = ["six"] }
        let req3 = req { optionalDependencies = [((3, 4), ["enum34"])] }
        specify "addDependency" $ do
            evalContext (addDependency "six") `shouldBe` req2
            evalContext (addDependency "six" >> addDependency "six")
                `shouldBe` req2
            evalContext (addDependency "nirum") `shouldBe`
                req { dependencies = ["nirum"] }
            evalContext (addDependency "six" >> addDependency "nirum")
                `shouldBe` req2 { dependencies = ["nirum", "six"] }
        specify "addOptionalDependency" $ do
            evalContext (addOptionalDependency (3, 4) "enum34") `shouldBe` req3
            evalContext ( addOptionalDependency (3, 4) "enum34"
                        >> addOptionalDependency (3, 4) "enum34"
                        )
                `shouldBe` req3
            evalContext (addOptionalDependency (3, 4) "ipaddress") `shouldBe`
                req { optionalDependencies = [((3, 4), ["ipaddress"])] }
            evalContext ( addOptionalDependency (3, 4) "enum34"
                        >> addOptionalDependency (3, 4) "ipaddress"
                        )
                `shouldBe` req
                    { optionalDependencies = [ ((3, 4), ["enum34", "ipaddress"])
                                             ]
                    }
            evalContext (addOptionalDependency (3, 5) "typing")
                `shouldBe` req { optionalDependencies = [((3, 5), ["typing"])] }
            evalContext ( addOptionalDependency (3, 4) "enum34"
                        >> addOptionalDependency (3, 5) "typing"
                        )
                `shouldBe` req3
                    { optionalDependencies = [ ((3, 4), ["enum34"])
                                             , ((3, 5), ["typing"])
                                             ]
                    }

spec :: Spec
spec = do
    spec'

    describe "toClassName" $ do
        it "transform the facial name of the argument into PascalCase" $ do
            toClassName "test" `shouldBe` "Test"
            toClassName "hello-world" `shouldBe` "HelloWorld"
        it "appends an underscore to the result if it's a reserved keyword" $ do
            toClassName "true" `shouldBe` "True_"
            toClassName "false" `shouldBe` "False_"
            toClassName "none" `shouldBe` "None_"

    describe "toAttributeName" $ do
        it "transform the facial name of the argument into snake_case" $ do
            toAttributeName "test" `shouldBe` "test"
            toAttributeName "hello-world" `shouldBe` "hello_world"
        it "appends an underscore to the result if it's a reserved keyword" $ do
            toAttributeName "def" `shouldBe` "def_"
            toAttributeName "lambda" `shouldBe` "lambda_"
            toAttributeName "nonlocal" `shouldBe` "nonlocal_"

    specify "toImportPath" $ do
        let (Source pkg _) = makeDummySource $ Module [] Nothing
            target' = target $ metadata pkg
        toImportPath target' ["foo", "bar"] `shouldBe` "foo.bar"

    describe "toImportPaths" $ do
        it "adds ancestors of packages" $ do
            let (Source pkg _) = makeDummySource $ Module [] Nothing
                modulePaths = keysSet $ modules pkg
                target' = target $ metadata pkg
            toImportPaths target' modulePaths `shouldBe`
                [ "foo"
                , "foo.bar"
                , "qux"
                ]
        it "applies renames before add ancestors of packages" $ do
            let (Source pkg _) = makeDummySource' [] (Module [] Nothing)
                                                  [(["foo"], ["f", "oo"])]
                modulePaths = keysSet $ modules pkg
                target' = target $ metadata pkg
            toImportPaths target' modulePaths `shouldBe`
                [ "f"  -- > "f" should be added
                , "f.oo"
                , "f.oo.bar"
                , "qux"
                ]

    specify "renameModulePath" $ do
        let renames = [ (["foo"], ["poo"])
                      , (["foo", "bar"], ["foo"])
                      , (["baz"], ["p", "az"])
                      ] :: RenameMap
        renameModulePath renames ["foo"] `shouldBe` ["poo"]
        renameModulePath renames ["foo", "baz"] `shouldBe` ["poo", "baz"]
        renameModulePath renames ["foo", "bar"] `shouldBe` ["foo"]
        renameModulePath renames ["foo", "bar", "qux"] `shouldBe` ["foo", "qux"]
        renameModulePath renames ["baz"] `shouldBe` ["p", "az"]
        renameModulePath renames ["baz", "qux"] `shouldBe` ["p", "az", "qux"]
        renameModulePath renames ["qux", "foo"] `shouldBe` ["qux", "foo"]

    specify "indent" $ do
        indent "    " ("a\n    b\n\nc\n" :: Code) `shouldBe`
            "    a\n        b\n\n    c\n"
        indent "    " ("\"\"\"foo\nbar\n\"\"\"" :: Code) `shouldBe`
            "    \"\"\"foo\n    bar\n    \"\"\""

    specify "stringLiteral" $ do
        stringLiteral "asdf" `shouldBe` [q|"asdf"|]
        stringLiteral [q|Say 'hello world'|]
            `shouldBe` [q|"Say 'hello world'"|]
        stringLiteral [q|Say "hello world"|]
            `shouldBe` [q|"Say \"hello world\""|]
        stringLiteral "Say '\xc548\xb155'"
            `shouldBe` [q|u"Say '\uc548\ub155'"|]
