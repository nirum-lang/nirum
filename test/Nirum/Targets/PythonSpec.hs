{-# LANGUAGE OverloadedLists, OverloadedStrings, QuasiQuotes,
             ScopedTypeVariables #-}
module Nirum.Targets.PythonSpec where

import Control.Monad (forM_)
import Data.Either (isRight)
import Data.Maybe (fromJust)

import qualified Data.Map.Strict as M
import qualified Data.SemVer as SV
import System.FilePath ((</>))
import Test.Hspec.Meta
import Text.Email.Validate (emailAddress)
import Text.InterpolatedString.Perl6 (q, qq)

import Nirum.Constructs.Annotation (empty)
import Nirum.Constructs.Identifier (Identifier)
import Nirum.Constructs.Module (Module (Module))
import Nirum.Constructs.ModulePath (ModulePath, fromIdentifiers)
import Nirum.Constructs.Name (Name (Name))
import Nirum.Constructs.TypeDeclaration ( Field (Field)
                                        , PrimitiveTypeIdentifier (..)
                                        , Type ( Alias
                                               , RecordType
                                               , UnboxedType
                                               )
                                        , TypeDeclaration ( Import
                                                          , TypeDeclaration
                                                          )
                                        )
import Nirum.Constructs.TypeExpression ( TypeExpression ( ListModifier
                                                        , MapModifier
                                                        , OptionModifier
                                                        , SetModifier
                                                        , TypeIdentifier
                                                        )
                                       )
import Nirum.Package (Package (metadata, modules), resolveBoundModule)
import Nirum.Package.Metadata ( Author (Author, email, name, uri)
                              , Metadata (Metadata
                                         , authors
                                         , target
                                         , version
                                         , description)
                              , Target (compilePackage)
                              )
import qualified Nirum.Package.ModuleSet as MS
import Nirum.PackageSpec (createPackage)
import qualified Nirum.Targets.Python as PY
import Nirum.Targets.Python ( Source (Source)
                            , CodeGen
                            , CodeGenContext ( localImports
                                             , standardImports
                                             , thirdPartyImports
                                             )
                            , InstallRequires ( InstallRequires
                                              , dependencies
                                              , optionalDependencies
                                              )
                            , Python (Python)
                            , PythonVersion (Python2, Python3)
                            , RenameMap
                            , addDependency
                            , addOptionalDependency
                            , compilePrimitiveType
                            , compileTypeExpression
                            , insertLocalImport
                            , insertStandardImport
                            , insertThirdPartyImports
                            , insertThirdPartyImportsA
                            , localImportsMap
                            , minimumRuntime
                            , parseModulePath
                            , renameModulePath
                            , runCodeGen
                            , stringLiteral
                            , toAttributeName
                            , toClassName
                            , toNamePair
                            , unionInstallRequires
                            )

codeGen :: a -> CodeGen a
codeGen = return

makeDummySource' :: [Identifier] -> Module -> RenameMap -> Source
makeDummySource' pathPrefix m renames =
    Source pkg $ fromJust $ resolveBoundModule ["foo"] pkg
  where
    mp :: [Identifier] -> ModulePath
    mp identifiers = fromJust $ fromIdentifiers (pathPrefix ++ identifiers)
    metadata' :: Metadata Python
    metadata' = Metadata
        { version = SV.version 1 2 3 [] []
        , authors =
              [ Author
                    { name = "John Doe"
                    , email = Just (fromJust $ emailAddress "john@example.com")
                    , uri = Nothing
                    }
              ]
        , description = Just "Package description"
        , target = Python "sample-package" minimumRuntime renames
        }
    pkg :: Package Python
    pkg = createPackage
            metadata'
            [ (mp ["foo"], m)
            , ( mp ["foo", "bar"]
              , Module [ Import (mp ["qux"]) "path" empty
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

spec :: Spec
spec = parallel $ forM_ ([Python2, Python3] :: [PythonVersion]) $ \ ver -> do
    let empty' = PY.empty ver
        -- run' :: CodeGen a -> (Either CompileError a, CodeGenContext)
        run' c = runCodeGen c empty'
        -- code :: CodeGen a -> a
        code = either (const undefined) id . fst . run'
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
            standardImports ctx `shouldBe` ["os", "sys"]
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
        specify "insertStandardImport" $ do
            let codeGen1 = insertStandardImport "sys"
            let (e1, ctx1) = runCodeGen codeGen1 empty'
            e1 `shouldSatisfy` isRight
            standardImports ctx1 `shouldBe` ["sys"]
            thirdPartyImports ctx1 `shouldBe` []
            localImports ctx1 `shouldBe` []
            compileError codeGen1 `shouldBe` Nothing
            let codeGen2 = codeGen1 >> insertStandardImport "os"
            let (e2, ctx2) = runCodeGen codeGen2 empty'
            e2 `shouldSatisfy` isRight
            standardImports ctx2 `shouldBe` ["os", "sys"]
            thirdPartyImports ctx2 `shouldBe` []
            localImports ctx2 `shouldBe` []
            compileError codeGen2 `shouldBe` Nothing

    specify [qq|compilePrimitiveType ($ver)|] $ do
        code (compilePrimitiveType Bool) `shouldBe` "bool"
        code (compilePrimitiveType Bigint) `shouldBe` "int"
        let (decimalCode, decimalContext) = run' (compilePrimitiveType Decimal)
        decimalCode `shouldBe` Right "decimal.Decimal"
        standardImports decimalContext `shouldBe` ["decimal"]
        code (compilePrimitiveType Int32) `shouldBe` "int"
        code (compilePrimitiveType Int64) `shouldBe`
            case ver of
                Python2 -> "numbers.Integral"
                Python3 -> "int"
        code (compilePrimitiveType Float32) `shouldBe` "float"
        code (compilePrimitiveType Float64) `shouldBe` "float"
        code (compilePrimitiveType Text) `shouldBe`
            case ver of
                Python2 -> "unicode"
                Python3 -> "str"
        code (compilePrimitiveType Binary) `shouldBe` "bytes"
        let (dateCode, dateContext) = run' (compilePrimitiveType Date)
        dateCode `shouldBe` Right "datetime.date"
        standardImports dateContext `shouldBe` ["datetime"]
        let (datetimeCode, datetimeContext) =
                run' (compilePrimitiveType Datetime)
        datetimeCode `shouldBe` Right "datetime.datetime"
        standardImports datetimeContext `shouldBe` ["datetime"]
        let (uuidCode, uuidContext) = run' (compilePrimitiveType Uuid)
        uuidCode `shouldBe` Right "uuid.UUID"
        standardImports uuidContext `shouldBe` ["uuid"]
        code (compilePrimitiveType Uri) `shouldBe`
            case ver of
                Python2 -> "unicode"
                Python3 -> "str"

    describe [qq|compileTypeExpression ($ver)|] $ do
        let s = makeDummySource $ Module [] Nothing
        specify "TypeIdentifier" $ do
            let (c, ctx) = run' $
                    compileTypeExpression s (TypeIdentifier "bigint")
            standardImports ctx `shouldBe` []
            localImports ctx `shouldBe` []
            c `shouldBe` Right "int"
        specify "OptionModifier" $ do
            let (c', ctx') = run' $
                    compileTypeExpression s (OptionModifier "int32")
            standardImports ctx' `shouldBe` ["typing"]
            localImports ctx' `shouldBe` []
            c' `shouldBe` Right "typing.Optional[int]"
        specify "SetModifier" $ do
            let (c'', ctx'') = run' $
                    compileTypeExpression s (SetModifier "int32")
            standardImports ctx'' `shouldBe` ["typing"]
            localImports ctx'' `shouldBe` []
            c'' `shouldBe` Right "typing.AbstractSet[int]"
        specify "ListModifier" $ do
            let (c''', ctx''') = run' $
                    compileTypeExpression s (ListModifier "int32")
            standardImports ctx''' `shouldBe` ["typing"]
            localImports ctx''' `shouldBe` []
            c''' `shouldBe` Right "typing.Sequence[int]"
        specify "MapModifier" $ do
            let (c'''', ctx'''') = run' $
                    compileTypeExpression s (MapModifier "uuid" "int32")
            standardImports ctx'''' `shouldBe` ["typing", "uuid"]
            localImports ctx'''' `shouldBe` []
            c'''' `shouldBe` Right "typing.Mapping[uuid.UUID, int]"

    describe [qq|toClassName ($ver)|] $ do
        it "transform the facial name of the argument into PascalCase" $ do
            toClassName "test" `shouldBe` "Test"
            toClassName "hello-world" `shouldBe` "HelloWorld"
        it "appends an underscore to the result if it's a reserved keyword" $ do
            toClassName "true" `shouldBe` "True_"
            toClassName "false" `shouldBe` "False_"
            toClassName "none" `shouldBe` "None_"

    describe [qq|toAttributeName ($ver)|] $ do
        it "transform the facial name of the argument into snake_case" $ do
            toAttributeName "test" `shouldBe` "test"
            toAttributeName "hello-world" `shouldBe` "hello_world"
        it "appends an underscore to the result if it's a reserved keyword" $ do
            toAttributeName "def" `shouldBe` "def_"
            toAttributeName "lambda" `shouldBe` "lambda_"
            toAttributeName "nonlocal" `shouldBe` "nonlocal_"

    describe [qq|toNamePair ($ver)|] $ do
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

    specify [qq|stringLiteral ($ver)|] $ do
        stringLiteral "asdf" `shouldBe` [q|"asdf"|]
        stringLiteral [q|Say 'hello world'|]
            `shouldBe` [q|"Say 'hello world'"|]
        stringLiteral [q|Say "hello world"|]
            `shouldBe` [q|"Say \"hello world\""|]
        stringLiteral "Say '\xc548\xb155'"
            `shouldBe` [q|u"Say '\uc548\ub155'"|]

    describe [qq|compilePackage ($ver)|] $ do
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

    describe [qq|InstallRequires ($ver)|] $ do
        let req = InstallRequires [] []
            req2 = req { dependencies = ["six"] }
            req3 = req { optionalDependencies = [((3, 4), ["enum34"])] }
        specify "addDependency" $ do
            addDependency req "six" `shouldBe` req2
            addDependency req2 "six" `shouldBe` req2
            addDependency req "nirum" `shouldBe`
                req { dependencies = ["nirum"] }
            addDependency req2 "nirum" `shouldBe`
                req2 { dependencies = ["nirum", "six"] }
        specify "addOptionalDependency" $ do
            addOptionalDependency req (3, 4) "enum34" `shouldBe` req3
            addOptionalDependency req3 (3, 4) "enum34" `shouldBe` req3
            addOptionalDependency req (3, 4) "ipaddress" `shouldBe`
                req { optionalDependencies = [((3, 4), ["ipaddress"])] }
            addOptionalDependency req3 (3, 4) "ipaddress" `shouldBe`
                req { optionalDependencies = [ ((3, 4), ["enum34", "ipaddress"])
                                             ]
                    }
            addOptionalDependency req (3, 5) "typing" `shouldBe`
                req { optionalDependencies = [((3, 5), ["typing"])] }
            addOptionalDependency req3 (3, 5) "typing" `shouldBe`
                req3 { optionalDependencies = [ ((3, 4), ["enum34"])
                                              , ((3, 5), ["typing"])
                                              ]
                     }
        specify "unionInstallRequires" $ do
            (req `unionInstallRequires` req) `shouldBe` req
            (req `unionInstallRequires` req2) `shouldBe` req2
            (req2 `unionInstallRequires` req) `shouldBe` req2
            (req `unionInstallRequires` req3) `shouldBe` req3
            (req3 `unionInstallRequires` req) `shouldBe` req3
            let req4 = req3 { dependencies = ["six"] }
            (req2 `unionInstallRequires` req3) `shouldBe` req4
            (req3 `unionInstallRequires` req2) `shouldBe` req4
            let req5 = req { dependencies = ["nirum"]
                           , optionalDependencies = [((3, 4), ["ipaddress"])]
                           }
                req6 = addOptionalDependency (addDependency req4 "nirum")
                                             (3, 4) "ipaddress"
            (req4 `unionInstallRequires` req5) `shouldBe` req6
            (req5 `unionInstallRequires` req4) `shouldBe` req6
    specify [qq|toImportPath ($ver)|] $ do
        let (Source pkg _) = makeDummySource $ Module [] Nothing
            target' = target $ metadata pkg
        PY.toImportPath target' ["foo", "bar"] `shouldBe` "foo.bar"
    describe [qq|toImportPath ($ver)|] $ do
        it "adds ancestors of packages" $ do
            let (Source pkg _) = makeDummySource $ Module [] Nothing
                modulePaths = MS.keysSet $ modules pkg
                target' = target $ metadata pkg
            PY.toImportPaths target' modulePaths `shouldBe`
                [ "foo"
                , "foo.bar"
                , "qux"
                ]
        it "applies renames before add ancestors of packages" $ do
            let (Source pkg _) = makeDummySource' [] (Module [] Nothing)
                                                  [(["foo"], ["f", "oo"])]
                modulePaths = MS.keysSet $ modules pkg
                target' = target $ metadata pkg
            PY.toImportPaths target' modulePaths `shouldBe`
                [ "f"  -- > "f" should be added
                , "f.oo"
                , "f.oo.bar"
                , "qux"
                ]
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
