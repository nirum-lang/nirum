{-# LANGUAGE OverloadedLists, QuasiQuotes, ScopedTypeVariables #-}
{- |
This unit test module optionally depends on Python interpreter.
It internally tries to popen python3 executable, and import nirum Python
package.  If any of these prerequisites are not satisfied, tests depending
on Python interpreter are skipped instead of marked failed.

To make Python interpreter possible to import nirum package, you need to
install the nirum package to your Python site-packages using pip command.
We recommend to install the package inside a virtualenv (pyvenv) and
run this unit test in the virtualenv (pyvenv).  E.g.:

> $ pyvenv env
> $ . env/bin/activate
> (env)$ pip install git+git://github.com/spoqa/nirum-python.git
> (env)$ cabal test  # or: stack test
-}
module Nirum.Targets.PythonSpec where

import Control.Monad (forM_, void, unless)
import Data.Char (isSpace)
import Data.Maybe (fromJust, isJust)
import System.IO.Error (catchIOError)

import Data.Either (isRight)
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as M
import qualified Data.SemVer as SV
import Data.Set (Set, union)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (isValid, takeDirectory, (</>))
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory)
import System.Process ( CreateProcess (cwd)
                      , proc
                      , readCreateProcess
                      , readCreateProcessWithExitCode
                      )
import Test.Hspec.Meta
import Text.Email.Validate (emailAddress)
import Text.InterpolatedString.Perl6 (q, qq)
import Text.Megaparsec (char, digitChar, runParser, some, space, string')
import Text.Megaparsec.String (Parser)

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
import Nirum.Package (BoundModule (modulePath), Package, resolveBoundModule)
import Nirum.Package.Metadata ( Author (Author, email, name, uri)
                              , Metadata (Metadata, authors, version)
                              )
import Nirum.PackageSpec (createPackage)
import qualified Nirum.Targets.Python as PY
import Nirum.Targets.Python ( Source (Source)
                            , Code
                            , CodeGen
                            , CodeGenContext ( localImports
                                             , standardImports
                                             , thirdPartyImports
                                             )
                            , CompileError
                            , InstallRequires ( InstallRequires
                                              , dependencies
                                              , optionalDependencies
                                              )
                            , PythonVersion (Python2, Python3)
                            , addDependency
                            , addOptionalDependency
                            , compilePackage
                            , compilePrimitiveType
                            , compileTypeExpression
                            , stringLiteral
                            , toAttributeName
                            , toClassName
                            , toImportPath
                            , toNamePair
                            , unionInstallRequires
                            , insertLocalImport
                            , insertStandardImport
                            , insertThirdPartyImports
                            , runCodeGen
                            )

codeGen :: a -> CodeGen a
codeGen = return

windows :: Bool
windows = os `elem` (["mingw32", "cygwin32", "win32"] :: [String])

data PyVersion = PyVersion Int Int Int deriving (Eq, Ord, Show)

installedPythonPaths :: Maybe FilePath -> IO [FilePath]
installedPythonPaths cwd' = do
    (exitCode, stdOut, _) <- readCreateProcessWithExitCode proc' ""
    return $ case exitCode of
        ExitSuccess -> filter isValid $ lines' stdOut
        _ -> []
  where
    proc' :: CreateProcess
    proc' = (if windows
             then proc "where.exe" ["python"]
             else proc "which" ["python3"]) { cwd = cwd' }
    lines' :: String -> [String]
    lines' = map T.unpack . filter (not . T.null)
                          . map T.strip
                          . T.lines
                          . T.pack

getPythonVersion :: Maybe FilePath -> FilePath -> IO (Maybe PyVersion)
getPythonVersion cwd' path' = do
    let proc' = (proc path' ["-V"]) { cwd = cwd' }
    pyVersionStr <- readCreateProcess proc' ""
    return $ case runParser pyVersionParser "<python3>" pyVersionStr of
            Left _ -> Nothing
            Right v -> Just v
  where
    pyVersionParser :: Parser PyVersion
    pyVersionParser = do
        void $ string' "python"
        space
        major <- integer
        void $ char '.'
        minor <- integer
        void $ char '.'
        micro <- integer
        return $ PyVersion major minor micro
    integer :: Parser Int
    integer = do
        digits <- some digitChar
        return (read digits :: Int)

findPython :: Maybe FilePath -> IO (Maybe FilePath)
findPython cwd' = installedPythonPaths cwd' >>= findPython'
  where
    findPython' :: [FilePath] -> IO (Maybe FilePath)
    findPython' (x : xs) = do
        pyVerM <- getPythonVersion cwd' x
        case pyVerM of
            Nothing -> findPython' xs
            Just version' -> if version' >= PyVersion 3 3 0
                             then return $ Just x
                             else findPython' xs
    findPython' [] = return Nothing

runPython' :: Maybe FilePath -> [String] -> String -> IO (Maybe String)
runPython' cwd' args stdinStr = do
    pyPathM <- findPython cwd'
    case pyPathM of
        Nothing -> do
            putStrLn "Python 3 seems not installed; skipping..."
            return Nothing
        Just path -> execute cwd' path args stdinStr
  where
    execute :: Maybe FilePath
            -> FilePath
            -> [String]
            -> String
            -> IO (Maybe String)
    execute cwdPath pyPath args' stdinStr' = do
        let proc' = (proc pyPath args') { cwd = cwdPath }
        result <- readCreateProcess proc' stdinStr'
        return $ Just result

runPython :: Maybe FilePath -> String -> IO (Maybe String)
runPython cwd' code' =
    catchIOError (runPython' cwd' [] code') $ \ e -> do
        putStrLn "\nThe following IO error was raised:\n"
        putStrLn $ indent "  " $ show e
        putStrLn "\n... while the following code was executed:\n"
        putStrLn $ indent "  " code'
        ioError e
  where
    indent :: String -> String -> String
    indent spaces content = unlines [spaces ++ l | l <- lines content]

testPythonSuit :: Maybe FilePath -> String -> T.Text -> IO ()
testPythonSuit cwd' suitCode testCode = do
    nirumPackageInstalledM <-
        runPython cwd' [q|
try: import nirum
except ImportError: print('F')
else: print('T')
            |]
    case nirumPackageInstalledM of
        Just nirumPackageInstalled ->
            case strip nirumPackageInstalled of
                "T" -> do
                    resultM <- runPython cwd' suitCode
                    case resultM of
                        Just result ->
                            unless (strip result == "True") $
                                expectationFailure $
                                T.unpack ("Test failed: " `T.append` testCode)
                        Nothing -> return ()
                _ -> putStrLn ("The nirum Python package cannot be " ++
                               "imported; skipped...")
        Nothing -> return ()
  where
    strip :: String -> String
    strip = dropWhile isSpace . dropWhileEnd isSpace

testPython :: Maybe FilePath -> T.Text -> T.Text -> IO ()
testPython cwd' defCode testCode = testPythonSuit cwd' code' testCode'
  where
    -- to workaround hlint's "Eta reduce" warning
    -- hlint seems unable to look inside qq string literal...
    testCode' :: T.Text
    testCode' = testCode
    code' :: String
    code' = [qq|$defCode

if __name__ == '__main__':
    print(bool($testCode'))
|]

testRaisePython :: Maybe FilePath -> T.Text -> T.Text -> T.Text -> IO ()
testRaisePython cwd' errorClassName defCode testCode =
    testPythonSuit cwd' code' testCode''
  where
    -- to workaround hlint's "Eta reduce" warning
    -- hlint seems unable to look inside qq string literal...
    testCode'' :: T.Text
    testCode'' = testCode
    code' :: String
    code' = [qq|$defCode

if __name__ == '__main__':
    try:
        $testCode''
    except $errorClassName:
        print(True)
    else:
        print(False)
|]

makeDummySource' :: [Identifier] -> Module -> Source
makeDummySource' pathPrefix m =
    Source pkg $ fromJust $ resolveBoundModule ["foo"] pkg
  where
    mp :: [Identifier] -> ModulePath
    mp identifiers = fromJust $ fromIdentifiers (pathPrefix ++ identifiers)
    metadata' :: Metadata
    metadata' = Metadata
        { version = SV.version 1 2 3 [] []
        , authors =
              [ Author
                    { name = "John Doe"
                    , email = Just (fromJust $ emailAddress "john@example.com")
                    , uri = Nothing
                    }
              ]
        }
    pkg :: Package
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
makeDummySource = makeDummySource' []

versions :: [(PythonVersion, Set Code)]
versions = [ (Python2, [])
           , (Python3, ["typing"])
           ]

spec :: Spec
spec = parallel $ forM_ versions $ \ (ver, typing) -> do
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
                    insertLocalImport ".." "Path"
            let (e, ctx) = runCodeGen c empty'
            e `shouldSatisfy` isRight
            standardImports ctx `shouldBe` ["os", "sys"]
            thirdPartyImports ctx `shouldBe`
                [("nirum", ["serialize_unboxed_type", "serialize_enum_type"])]
            localImports ctx `shouldBe` [("..", ["Gender", "Path"])]
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

    specify "compilePrimitiveType" $ do
        code (compilePrimitiveType Bool) `shouldBe` "bool"
        code (compilePrimitiveType Bigint) `shouldBe` "int"
        let (decimalCode, decimalContext) = run' (compilePrimitiveType Decimal)
        decimalCode `shouldBe` Right "decimal.Decimal"
        standardImports decimalContext `shouldBe` ["decimal"]
        code (compilePrimitiveType Int32) `shouldBe` "int"
        code (compilePrimitiveType Int64) `shouldBe`
            case ver of
                Python2 -> "long"
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

    describe "compileTypeExpression" $ do
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
            standardImports ctx' `shouldBe` typing
            localImports ctx' `shouldBe` []
            c' `shouldBe` Right "typing.Optional[int]"
        specify "SetModifier" $ do
            let (c'', ctx'') = run' $
                    compileTypeExpression s (SetModifier "int32")
            standardImports ctx'' `shouldBe` typing
            localImports ctx'' `shouldBe` []
            c'' `shouldBe` Right "typing.AbstractSet[int]"
        specify "ListModifier" $ do
            let (c''', ctx''') = run' $
                    compileTypeExpression s (ListModifier "int32")
            standardImports ctx''' `shouldBe` typing
            localImports ctx''' `shouldBe` []
            c''' `shouldBe` Right "typing.Sequence[int]"
        specify "MapModifier" $ do
            let (c'''', ctx'''') = run' $
                    compileTypeExpression s (MapModifier "uuid" "int32")
            standardImports ctx'''' `shouldBe` union ["uuid"] typing
            localImports ctx'''' `shouldBe` []
            c'''' `shouldBe` Right "typing.Mapping[uuid.UUID, int]"

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

    specify "stringLiteral" $ do
        stringLiteral "asdf" `shouldBe` [q|"asdf"|]
        stringLiteral [q|Say 'hello world'|]
            `shouldBe` [q|"Say 'hello world'"|]
        stringLiteral [q|Say "hello world"|]
            `shouldBe` [q|"Say \"hello world\""|]
        stringLiteral "Say '\xc548\xb155'"
            `shouldBe` [q|u"Say '\uc548\ub155'"|]

    let test testRunner (Source pkg boundM) testCode =
            case errors of
                error' : _ -> fail $ T.unpack error'
                [] -> withSystemTempDirectory "nirumpy." $ \ dir -> do
                    forM_ codes $ \ (filePath, code') -> do
                        let filePath' = dir </> filePath
                            dirName = takeDirectory filePath'
                        createDirectoryIfMissing True dirName
                        TI.writeFile filePath' code'
                        {-- <- Remove '{' to print debug log
                        TI.putStrLn $ T.pack filePath'
                        TI.putStrLn code'
                        -- -}
                    testRunner (Just dir) defCode testCode
          where
            files :: M.Map FilePath (Either CompileError Code)
            files = compilePackage pkg
            errors :: [CompileError]
            errors = [error' | (_, Left error') <- M.toList files]
            codes :: [(FilePath, Code)]
            codes = [(path, code') | (path, Right code') <- M.toList files]
            pyImportPath :: T.Text
            pyImportPath = toImportPath $ modulePath boundM
            defCode :: Code
            defCode = [qq|from $pyImportPath import *|]

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
                    ]
            M.keysSet files `shouldBe` directoryStructure
        it "creates an emtpy Python package directory if necessary" $ do
            let (Source pkg _) = makeDummySource' ["test"] $ Module [] Nothing
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
                    ]
            M.keysSet files `shouldBe` directoryStructure
        specify "setup.py" $ do
            let setupPyFields = [ ("--name", "TestPackage")
                                , ("--author", "John Doe")
                                , ("--author-email", "john@example.com")
                                , ("--version", "1.2.3")
                                , ("--provides", "foo\nfoo.bar\nqux")
                                , ("--requires", "nirum")
                                ] :: [(String, T.Text)]
                source = makeDummySource $ Module [] Nothing
                testRunner cwd' _ _ =
                    {-- -- <- remove '{' to print debug log
                    do
                        let spath = case cwd' of
                                        Just c -> c </> "setup.py"
                                        Nothing -> "setup.py"
                        contents <- TI.readFile spath
                        TI.putStrLn "=========== setup.py ==========="
                        TI.putStrLn contents
                        TI.putStrLn "=========== /setup.py =========="
                    -- -}
                        forM_ setupPyFields $ \ (option, expected) -> do
                            out <- runPython' cwd' ["setup.py", option] ""
                            out `shouldSatisfy` isJust
                            let Just result = out
                            T.strip (T.pack result) `shouldBe` expected
            test testRunner source T.empty

    describe "InstallRequires" $ do
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

{-# ANN module ("HLint: ignore Functor law" :: String) #-}
{-# ANN module ("HLint: ignore Monad law, left identity" :: String) #-}
{-# ANN module ("HLint: ignore Monad law, right identity" :: String) #-}
{-# ANN module ("HLint: ignore Use >=>" :: String) #-}
