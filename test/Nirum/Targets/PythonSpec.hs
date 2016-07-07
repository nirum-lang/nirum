{-# LANGUAGE OverloadedLists, OverloadedStrings, QuasiQuotes,
             ScopedTypeVariables #-}
{-|
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
import Data.Maybe (fromJust)
import System.IO.Error (catchIOError)

import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess(cwd), proc, readCreateProcess)
import Test.Hspec.Meta
import Text.InterpolatedString.Perl6 (q, qq)
import Text.Megaparsec (char, digitChar, runParser, some, space, string')
import Text.Megaparsec.String (Parser)

import Nirum.Constructs.DeclarationSet (DeclarationSet)
import Nirum.Constructs.Module (Module(Module))
import Nirum.Constructs.Name (Name(Name))
import Nirum.Constructs.TypeDeclaration ( Field(Field)
                                        , EnumMember(EnumMember)
                                        , PrimitiveTypeIdentifier(..)
                                        , Tag(Tag)
                                        , Type( Alias
                                              , BoxedType
                                              , EnumType
                                              , RecordType
                                              , UnionType
                                              )
                                        , TypeDeclaration( Import
                                                         , TypeDeclaration
                                                         )
                                        )
import Nirum.Constructs.TypeExpression ( TypeExpression( ListModifier
                                                       , MapModifier
                                                       , OptionModifier
                                                       , SetModifier
                                                       , TypeIdentifier
                                                       )
                                       )
import Nirum.Package (BoundModule(modulePath), Package, resolveBoundModule)
import Nirum.PackageSpec (createPackage)
import Nirum.Targets.Python ( Source(Source, sourceModule, sourcePackage)
                            , CodeGen( code
                                     , localImports
                                     , standardImports
                                     , thirdPartyImports
                                     )
                            , compileError
                            , compilePackage
                            , compilePrimitiveType
                            , compileTypeExpression
                            , hasError
                            , toAttributeName
                            , toClassName
                            , toImportPath
                            , withLocalImport
                            , withStandardImport
                            , withThirdPartyImports
                            )

codeGen :: a -> CodeGen a
codeGen = return

windows :: Bool
windows = os `elem` (["mingw32", "cygwin32", "win32"] :: [String])

data PyVersion = PyVersion Int Int Int deriving (Eq, Ord, Show)

isPythonInstalled :: Maybe FilePath -> IO Bool
isPythonInstalled cwd' = do
    pyExist <- readCreateProcess proc' ""
    return $ not $ all isSpace pyExist
  where
    which :: String
    which = if windows then "where" else "which"
    proc' :: CreateProcess
    proc' = (proc which ["python3"]) { cwd = cwd' }

getPythonVersion :: Maybe FilePath -> IO (Maybe PyVersion)
getPythonVersion cwd' = do
    installed <- isPythonInstalled cwd'
    if installed then do
        let proc' = (proc "python3" ["-V"]) { cwd = cwd' }
        pyVersionStr <- readCreateProcess proc' ""
        return $ case runParser pyVersionParser "<python3>" pyVersionStr of
             Left _ -> Nothing
             Right v -> Just v
    else do
        putStrLn "Python 3 seems not installed; skipping..."
        return Nothing
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

runPython :: Maybe FilePath -> String -> IO (Maybe String)
runPython cwd' code' = do
    pyVerM <- getPythonVersion cwd'
    case pyVerM of
        Nothing -> do
            putStrLn "Can't determine Python version; skipping..."
            return Nothing
        Just version ->
            if version < PyVersion 3 3 0 then do
                putStrLn "Python seems below 3.3; skipping..."
                return Nothing
            else
                catchIOError (execute cwd' code') $ \e -> do
                    putStrLn "\nThe following IO error was raised:\n"
                    putStrLn $ indent "  " $ show e
                    putStrLn "\n... while the following code was executed:\n"
                    putStrLn $ indent "  " code'
                    ioError e
  where
    execute :: Maybe FilePath -> String -> IO (Maybe String)
    execute cwdPath pyCode = do
        let proc' = (proc "python3" []) { cwd = cwdPath }
        result <- readCreateProcess proc' pyCode
        return $ Just result
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
                _ -> putStrLn "The nirum Python package cannot be imported; \
                              \skipped..."
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

makeDummySource :: Module -> Source
makeDummySource m =
    Source pkg $ fromJust $ resolveBoundModule ["foo"] pkg
  where
    pkg :: Package
    pkg = createPackage
            [ (["foo"], m)
            , ( ["foo", "bar"]
              , Module [ Import ["qux"] "path"
                       , TypeDeclaration "path-box" (BoxedType "path") Nothing
                       , TypeDeclaration "int-box" (BoxedType "bigint") Nothing
                       , TypeDeclaration "point"
                                         (RecordType [ Field "x" "int64" Nothing
                                                     , Field "y" "int64" Nothing
                                                     ])
                                         Nothing
                       ] Nothing
              )
            , ( ["qux"]
              , Module [TypeDeclaration "path" (Alias "text") Nothing] Nothing
              )
            ]

spec :: Spec
spec = do
    describe "CodeGen" $ do
        let v = 123 :: Int
            cg = return v :: CodeGen Int
            f = return . g :: Int -> CodeGen Int
            f' = return . h :: Int -> CodeGen Int
            g = (+ 5) :: Int -> Int
            h = (* 2) :: Int -> Int
            g' = pure g :: CodeGen (Int -> Int)
            h' = pure h :: CodeGen (Int -> Int)
            id' x = x
        context "Functor" $ do
            specify "identity morphisms" $
                fmap id' (return 123 :: CodeGen Int) `shouldBe` id' (return 123)
            specify "composition of morphisms" $ do
                fmap (g . h) cg `shouldBe` (fmap g . fmap h) cg
                fmap (h . g) cg `shouldBe` (fmap h . fmap g) cg
        context "Applicative" $ do
            specify "identity law" $
                (pure id' <*> cg) `shouldBe` cg
            specify "homomorphism" $ do
                let pure' = pure :: a -> CodeGen a
                (pure g <*> pure v) `shouldBe` pure' (g v)
                (pure h <*> pure v) `shouldBe` pure' (h v)
            specify "interchange" $ do
                (g' <*> pure v) `shouldBe` (pure ($ v) <*> g')
                (h' <*> pure v) `shouldBe` (pure ($ v) <*> h')
            specify "composition" $ do
                (g' <*> (h' <*> cg)) `shouldBe` (pure (.) <*> g' <*> h' <*> cg)
                (h' <*> (g' <*> cg)) `shouldBe` (pure (.) <*> h' <*> g' <*> cg)
        context "Monad" $ do
            specify "left identity" $ do
                (return v >>= f) `shouldBe` f v
                (return v >>= f') `shouldBe` f' v
            specify "right identity" $
                (cg >>= return) `shouldBe` cg
            specify "associativity" $ do
                ((cg >>= f) >>= f') `shouldBe` (cg >>= (\x -> f x >>= f'))
                ((cg >>= f') >>= f) `shouldBe` (cg >>= (\x -> f' x >>= f))
            specify "packages and imports" $ do
                let (c :: CodeGen Int) = do
                        a <- withStandardImport "sys" cg
                        b <- withThirdPartyImports
                            [("nirum", ["serialize_boxed_type"])]
                            cg
                        c' <- withLocalImport ".." "Gender" cg
                        d <- withStandardImport "os" cg
                        e <- withThirdPartyImports
                            [("nirum", ["serialize_enum_type"])]
                            cg
                        f'' <- withLocalImport ".." "Path" cg
                        return $ sum ([a, b, c', d, e, f''] :: [Int])
                c `shouldSatisfy` (not . hasError)
                standardImports c `shouldBe` ["os", "sys"]
                thirdPartyImports c `shouldBe`
                    [("nirum", ["serialize_boxed_type", "serialize_enum_type"])]
                localImports c `shouldBe` [("..", ["Gender", "Path"])]
                code c `shouldBe` (123 * 6)
        specify "withStandardImport" $ do
            let codeGen1 = withStandardImport "sys" (pure True)
            codeGen1 `shouldSatisfy` (not . hasError)
            standardImports codeGen1 `shouldBe` ["sys"]
            thirdPartyImports codeGen1 `shouldBe` []
            localImports codeGen1 `shouldBe` []
            code codeGen1 `shouldBe` True
            compileError codeGen1 `shouldBe` Nothing
            let codeGen2 = withStandardImport "os" codeGen1
            codeGen2 `shouldSatisfy` (not . hasError)
            standardImports codeGen2 `shouldBe` ["os", "sys"]
            thirdPartyImports codeGen2 `shouldBe` []
            localImports codeGen2 `shouldBe` []
            code codeGen2 `shouldBe` True
            compileError codeGen2 `shouldBe` Nothing
        specify "fail" $ do
            let codeGen' = do
                    val <- withStandardImport "sys" (pure True)
                    _ <- fail "test"
                    withStandardImport "sys" (pure val)
            compileError codeGen' `shouldBe` Just "test"

    specify "compilePrimitiveType" $ do
        code (compilePrimitiveType Bool) `shouldBe` "bool"
        code (compilePrimitiveType Bigint) `shouldBe` "int"
        let decimal = compilePrimitiveType Decimal
        code decimal `shouldBe` "decimal.Decimal"
        standardImports decimal `shouldBe` ["decimal"]
        code (compilePrimitiveType Int32) `shouldBe` "int"
        code (compilePrimitiveType Int64) `shouldBe` "int"
        code (compilePrimitiveType Float32) `shouldBe` "float"
        code (compilePrimitiveType Float64) `shouldBe` "float"
        code (compilePrimitiveType Text) `shouldBe` "str"
        code (compilePrimitiveType Binary) `shouldBe` "bytes"
        let date = compilePrimitiveType Date
        code date `shouldBe` "datetime.date"
        standardImports date `shouldBe` ["datetime"]
        let datetime = compilePrimitiveType Datetime
        code datetime `shouldBe` "datetime.datetime"
        standardImports datetime `shouldBe` ["datetime"]
        let uuid = compilePrimitiveType Uuid
        code uuid `shouldBe` "uuid.UUID"
        standardImports uuid `shouldBe` ["uuid"]
        code (compilePrimitiveType Uri) `shouldBe` "str"

    describe "compileTypeExpression" $ do
        let s = makeDummySource $ Module [] Nothing
        specify "TypeIdentifier" $ do
            let c = compileTypeExpression s (TypeIdentifier "bigint")
            c `shouldSatisfy` (not . hasError)
            standardImports c `shouldBe` []
            localImports c `shouldBe` []
            code c `shouldBe` "int"
        specify "OptionModifier" $ do
            let c' = compileTypeExpression s (OptionModifier "text")
            c' `shouldSatisfy` (not . hasError)
            standardImports c' `shouldBe` ["typing"]
            localImports c' `shouldBe` []
            code c' `shouldBe` "typing.Optional[str]"
        specify "SetModifier" $ do
            let c'' = compileTypeExpression s (SetModifier "text")
            c'' `shouldSatisfy` (not . hasError)
            standardImports c'' `shouldBe` ["typing"]
            localImports c'' `shouldBe` []
            code c'' `shouldBe` "typing.AbstractSet[str]"
        specify "ListModifier" $ do
            let c''' = compileTypeExpression s (ListModifier "text")
            c''' `shouldSatisfy` (not . hasError)
            standardImports c''' `shouldBe` ["typing"]
            localImports c''' `shouldBe` []
            code c''' `shouldBe` "typing.Sequence[str]"
        specify "MapModifier" $ do
            let c'''' = compileTypeExpression s (MapModifier "uuid" "text")
            c'''' `shouldSatisfy` (not . hasError)
            standardImports c'''' `shouldBe` ["uuid", "typing"]
            localImports c'''' `shouldBe` []
            code c'''' `shouldBe` "typing.Mapping[uuid.UUID, str]"

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

    let test testRunner
             Source { sourcePackage = pkg
                    , sourceModule = boundM
                    }
             testCode =
            let files = compilePackage pkg
                errors = [error' | (_, Left error') <- M.toList files]
                codes = [(path, code') | (path, Right code') <- M.toList files]
                pyImportPath = toImportPath $ modulePath boundM
                defCode = [qq|from $pyImportPath import *|]
            in
                case errors of
                    error':_ -> fail $ T.unpack error'
                    [] -> withSystemTempDirectory "nirumpy." $ \dir -> do
                        forM_ codes $ \(filePath, code') -> do
                            let filePath' = dir </> filePath
                                dirName = takeDirectory filePath'
                            createDirectoryIfMissing True dirName
                            TI.writeFile filePath' code'
                        testRunner (Just dir) defCode testCode
        tM = test testPython
        tT' typeDecls = tM $ makeDummySource $ Module typeDecls Nothing
        tT typeDecl = tT' [typeDecl]
        tR source excType = test (`testRaisePython` excType) source
        tR'' typeDecls = tR $ makeDummySource $ Module typeDecls Nothing
        tR' typeDecl = tR'' [typeDecl]

    describe "compilePackage" $ do
        specify "boxed type" $ do
            let decl = TypeDeclaration "float-box" (BoxedType "float64") Nothing
            tT decl "isinstance(FloatBox, type)"
            tT decl "FloatBox(3.14).value == 3.14"
            tT decl "FloatBox(3.14) == FloatBox(3.14)"
            tT decl "FloatBox(3.14) != FloatBox(1.0)"
            tT decl [q|{FloatBox(3.14), FloatBox(3.14), FloatBox(1.0)} ==
                       {FloatBox(3.14), FloatBox(1.0)}|]
            tT decl "FloatBox(3.14).__nirum_serialize__() == 3.14"
            tT decl "FloatBox.__nirum_deserialize__(3.14) == FloatBox(3.14)"
            tT decl "FloatBox.__nirum_deserialize__(3.14) == FloatBox(3.14)"
            tR' decl "TypeError" "FloatBox.__nirum_deserialize__('a')"
            tR' decl "TypeError" "FloatBox('a')"
            let decls = [ Import ["foo", "bar"] "path-box"
                        , TypeDeclaration "imported-type-box"
                                          (BoxedType "path-box") Nothing
                        ]
            tT' decls "isinstance(ImportedTypeBox, type)"
            tT' decls [q|ImportedTypeBox(PathBox('/path/string')).value.value ==
                         '/path/string'|]
            tT' decls [q|ImportedTypeBox(PathBox('/path/string')) ==
                         ImportedTypeBox(PathBox('/path/string'))|]
            tT' decls [q|ImportedTypeBox(PathBox('/path/string')) !=
                         ImportedTypeBox(PathBox('/other/path'))|]
            tT' decls [q|{ImportedTypeBox(PathBox('/path/string')),
                          ImportedTypeBox(PathBox('/path/string')),
                          ImportedTypeBox(PathBox('/other/path')),
                          ImportedTypeBox(PathBox('/path/string')),
                          ImportedTypeBox(PathBox('/other/path'))} ==
                         {ImportedTypeBox(PathBox('/path/string')),
                          ImportedTypeBox(PathBox('/other/path'))}|]
            tT' decls [q|
                ImportedTypeBox(PathBox('/path/string')).__nirum_serialize__()
                == '/path/string'
            |]
            tT' decls [q|
                ImportedTypeBox.__nirum_deserialize__('/path/string') ==
                ImportedTypeBox(PathBox('/path/string'))
            |]
            tR'' decls "TypeError" "ImportedTypeBox.__nirum_deserialize__(123)"
            tR'' decls "TypeError" "ImportedTypeBox(123)"
        specify "enum type" $ do
            let members = [ "male"
                          , EnumMember (Name "female" "yeoseong") Nothing
                          ] :: DeclarationSet EnumMember
                decl = TypeDeclaration "gender" (EnumType members) Nothing
            tT decl "type(Gender) is enum.EnumMeta"
            tT decl "set(Gender) == {Gender.male, Gender.female}"
            tT decl "Gender.male.value == 'male'"
            tT decl "Gender.female.value == 'yeoseong'"
            tT decl "Gender.__nirum_deserialize__('male') == Gender.male"
            tT decl "Gender.__nirum_deserialize__('yeoseong') == Gender.female"
            tR' decl "ValueError" "Gender.__nirum_deserialize__('namja')"
            tT decl "Gender.male.__nirum_serialize__() == 'male'"
            tT decl "Gender.female.__nirum_serialize__() == 'yeoseong'"
            let members' = [ "soryu-asuka-langley"
                           , "ayanami-rei"
                           , "ikari-shinji"
                           , "katsuragi-misato"
                           , "nagisa-kaworu"
                           ] :: DeclarationSet EnumMember
                decl' = TypeDeclaration "eva-char" (EnumType members') Nothing
            tT decl' "type(EvaChar) is enum.EnumMeta"
            tT decl' "set(EvaChar) == {EvaChar.soryu_asuka_langley, \
                                     \ EvaChar.ayanami_rei, \
                                     \ EvaChar.ikari_shinji, \
                                     \ EvaChar.katsuragi_misato, \
                                     \ EvaChar.nagisa_kaworu}"
            tT decl' "EvaChar.soryu_asuka_langley.value=='soryu_asuka_langley'"
            tT decl' "EvaChar.soryu_asuka_langley.__nirum_serialize__() == \
                     \ 'soryu_asuka_langley'"
            tT decl' "EvaChar.__nirum_deserialize__('soryu_asuka_langley') == \
                     \ EvaChar.soryu_asuka_langley"
            tT decl' "EvaChar.__nirum_deserialize__('soryu-asuka-langley') == \
                     \ EvaChar.soryu_asuka_langley"  -- to be robust
        specify "record type" $ do
            let fields = [ Field (Name "left" "x") "bigint" Nothing
                         , Field "top" "bigint" Nothing
                         ]
                payload = "{'_type': 'point', 'x': 3, 'top': 14}" :: T.Text
                decl = TypeDeclaration "point" (RecordType fields) Nothing
            tT decl "isinstance(Point, type)"
            tT decl "Point(left=3, top=14).left == 3"
            tT decl "Point(left=3, top=14).top == 14"
            tT decl "Point(left=3, top=14) == Point(left=3, top=14)"
            tT decl "Point(left=3, top=14) != Point(left=3, top=15)"
            tT decl "Point(left=3, top=14) != Point(left=4, top=14)"
            tT decl "Point(left=3, top=14) != Point(left=4, top=15)"
            tT decl "Point(left=3, top=14) != 'foo'"
            tT decl [q|Point(left=3, top=14).__nirum_serialize__() ==
                       {'_type': 'point', 'x': 3, 'top': 14}|]
            tT decl [qq|Point.__nirum_deserialize__($payload) ==
                        Point(left=3, top=14)|]
            tR' decl "ValueError"
                "Point.__nirum_deserialize__({'x': 3, 'top': 14})"
            tR' decl "ValueError"
                "Point.__nirum_deserialize__({'_type': 'foo'})"
            tR' decl "TypeError" "Point(left=1, top='a')"
            tR' decl "TypeError" "Point(left='a', top=1)"
            tR' decl "TypeError" "Point(left='a', top='b')"
            let fields' = [ Field "left" "int-box" Nothing
                          , Field "top" "int-box" Nothing
                          ]
                decls = [ Import ["foo", "bar"] "int-box"
                        , TypeDeclaration "point" (RecordType fields') Nothing
                        ]
                payload' = "{'_type': 'point', 'left': 3, 'top': 14}" :: T.Text
            tT' decls "isinstance(Point, type)"
            tT' decls "Point(left=IntBox(3), top=IntBox(14)).left == IntBox(3)"
            tT' decls "Point(left=IntBox(3), top=IntBox(14)).top == IntBox(14)"
            tT' decls [q|Point(left=IntBox(3), top=IntBox(14)) ==
                         Point(left=IntBox(3), top=IntBox(14))|]
            tT' decls [q|Point(left=IntBox(3), top=IntBox(14)) !=
                         Point(left=IntBox(3), top=IntBox(15))|]
            tT' decls [q|Point(left=IntBox(3), top=IntBox(14)) !=
                         Point(left=IntBox(4), top=IntBox(14))|]
            tT' decls [q|Point(left=IntBox(3), top=IntBox(14)) !=
                         Point(left=IntBox(4), top=IntBox(15))|]
            tT' decls "Point(left=IntBox(3), top=IntBox(14)) != 'foo'"
            tT' decls [q|Point(left=IntBox(3),
                               top=IntBox(14)).__nirum_serialize__() ==
                         {'_type': 'point', 'left': 3, 'top': 14}|]
            tT' decls [qq|Point.__nirum_deserialize__($payload') ==
                          Point(left=IntBox(3), top=IntBox(14))|]
            tR'' decls "ValueError"
                 "Point.__nirum_deserialize__({'left': 3, 'top': 14})"
            tR'' decls "ValueError"
                 "Point.__nirum_deserialize__({'_type': 'foo'})"
            tR'' decls "TypeError" "Point(left=IntBox(1), top='a')"
            tR'' decls "TypeError" "Point(left=IntBox(1), top=2)"
            let fields'' = [ Field "xy" "point" Nothing
                           , Field "z" "int64" Nothing
                           ]
                decls' = [ Import ["foo", "bar"] "point"
                         , TypeDeclaration "point3d"
                                           (RecordType fields'')
                                           Nothing
                         ]
            tT' decls' "isinstance(Point3d, type)"
            tT' decls' [q|Point3d(xy=Point(x=1, y=2), z=3).xy ==
                          Point(x=1, y=2)|]
            tT' decls' "Point3d(xy=Point(x=1, y=2), z=3).xy.x == 1"
            tT' decls' "Point3d(xy=Point(x=1, y=2), z=3).xy.y == 2"
            tT' decls' "Point3d(xy=Point(x=1, y=2), z=3).z == 3"
            tT' decls' [q|Point3d(xy=Point(x=1, y=2), z=3).__nirum_serialize__()
                          == {'_type': 'point3d',
                              'xy': {'_type': 'point', 'x': 1, 'y': 2},
                              'z': 3}|]
            tT' decls' [q|Point3d.__nirum_deserialize__({
                              '_type': 'point3d',
                              'xy': {'_type': 'point', 'x': 1, 'y': 2},
                              'z': 3
                          }) == Point3d(xy=Point(x=1, y=2), z=3)|]
        specify "record type with one field" $ do
            let fields = [ Field "length" "bigint" Nothing ]
                payload = "{'_type': 'line', 'length': 3}" :: T.Text
                decl = TypeDeclaration "line" (RecordType fields) Nothing
            tT decl "isinstance(Line, type)"
            tT decl "Line(length=10).length == 10"
            tT decl "Line.__slots__ == ('length', )"
            tT decl [qq|Line(length=3).__nirum_serialize__() == $payload|]
        specify "union type" $ do
            let wasternNameTag =
                    Tag "western-name" [ Field "first-name" "text" Nothing
                                       , Field "middle-name" "text" Nothing
                                       , Field "last-name" "text" Nothing
                                       ] Nothing
                eastAsianNameTag =
                    Tag "east-asian-name" [ Field "family-name" "text" Nothing
                                          , Field "given-name" "text" Nothing
                                          ] Nothing
                cultureAgnosticNameTag =
                    Tag "culture-agnostic-name"
                        [ Field "fullname" "text" Nothing ]
                        Nothing
                tags = [ wasternNameTag
                       , eastAsianNameTag
                       , cultureAgnosticNameTag
                       ]
                decl = TypeDeclaration "name" (UnionType tags) Nothing
            tT decl "isinstance(Name, type)"
            tT decl "Name.Tag.western_name.value == 'western_name'"
            tT decl "Name.Tag.east_asian_name.value == 'east_asian_name'"
            tT decl [q|Name.Tag.culture_agnostic_name.value ==
                       'culture_agnostic_name'|]
            tT decl "isinstance(WesternName, type)"
            tT decl "issubclass(WesternName, Name)"
            tR' decl "NotImplementedError" "Name()"
            tT decl [q|WesternName(first_name='foo', middle_name='bar',
                                   last_name='baz').first_name == 'foo'|]
            tT decl [q|WesternName(first_name='foo', middle_name='bar',
                                   last_name='baz').middle_name == 'bar'|]
            tT decl [q|WesternName(first_name='foo', middle_name='bar',
                                   last_name='baz').last_name == 'baz'|]
            tR' decl "TypeError" [q|WesternName(first_name=1,middle_name='bar',
                                                last_name='baz')|]
            tR' decl "TypeError" [q|WesternName(first_name='foo',
                                                middle_name=1,
                                                last_name='baz')|]
            tR' decl "TypeError" [q|WesternName(first_name='foo',
                                                middle_name='bar',
                                                last_name=1)|]
            tT decl [q|WesternName(first_name='foo', middle_name='bar',
                                   last_name='baz') ==
                       WesternName(first_name='foo', middle_name='bar',
                                   last_name='baz')
                    |]
            tT decl [q|WesternName(first_name='wrong',
                                   middle_name='bar', last_name='baz') !=
                       WesternName(first_name='foo', middle_name='bar',
                                   last_name='baz')
                    |]
            tT decl [q|WesternName(first_name='foo', middle_name='wrong',
                                    last_name='baz') !=
                       WesternName(first_name='foo', middle_name='bar',
                                   last_name='baz')
                    |]
            tT decl [q|WesternName(first_name='foo', middle_name='bar',
                                   last_name='wrong') !=
                       WesternName(first_name='foo', middle_name='bar',
                                   last_name='baz')
                    |]
            tT decl [q|WesternName(first_name='wrong', middle_name='wrong',
                                   last_name='wrong') !=
                       WesternName(first_name='foo', middle_name='bar',
                                   last_name='baz')|]
            tT decl "isinstance(EastAsianName, type)"
            tT decl "issubclass(EastAsianName, Name)"
            tT decl [q|EastAsianName(family_name='foo',
                                     given_name='baz').family_name == 'foo'|]
            tT decl [q|EastAsianName(family_name='foo',
                                     given_name='baz').given_name == 'baz'|]
            tT decl [q|EastAsianName(family_name='foo', given_name='baz') ==
                       EastAsianName(family_name='foo', given_name='baz')|]
            tT decl [q|EastAsianName(family_name='foo',
                                     given_name='wrong') !=
                       EastAsianName(family_name='foo', given_name='baz')|]
            tT decl [q|EastAsianName(family_name='wrong', given_name='baz') !=
                       EastAsianName(family_name='foo', given_name='baz')|]
            tT decl [q|EastAsianName(family_name='wrong',
                                     given_name='wrong') !=
                       EastAsianName(family_name='foo', given_name='baz')|]
            tR'
                decl
                "TypeError"
                "EastAsianName(family_name=1, given_name='baz')"
            tR'
                decl
                "TypeError"
                "EastAsianName(family_name='foo', given_name=2)"
            tT decl "isinstance(CultureAgnosticName, type)"
            tT decl "issubclass(CultureAgnosticName, Name)"
            tT
                decl
                "CultureAgnosticName(fullname='foobar').fullname == 'foobar'"
            tT decl [q|CultureAgnosticName(fullname='foobar') ==
                       CultureAgnosticName(fullname='foobar')|]
            tT decl [q|CultureAgnosticName(fullname='wrong') !=
                       CultureAgnosticName(fullname='foobar')|]
            tR' decl "TypeError" "CultureAgnosticName(fullname=1)"
        specify "union type with one tag" $ do
            let cultureAgnosticNameTag =
                    Tag "pop"
                        [ Field "country" "text" Nothing ]
                        Nothing
                tags = [cultureAgnosticNameTag]
                decl = TypeDeclaration "music" (UnionType tags) Nothing
            tT decl "Pop(country='KR').country == 'KR'"
            tT decl "Pop(country='KR') == Pop(country='KR')"
            tT decl "Pop(country='US') != Pop(country='KR')"
            tR' decl "TypeError" "Pop(country=1)"
            tT decl "Pop.__slots__ == ('country', )"
        specify "union type with behind names" $ do
            let pop =
                    Tag (Name "pop" "popular_music")
                        [ Field "country" "text" Nothing ]
                        Nothing
                tags = [pop]
                decl = TypeDeclaration "music" (UnionType tags) Nothing
            tT decl "Pop(country='KR').__nirum_tag__.value == 'popular_music'"



{-# ANN module ("HLint: ignore Functor law" :: String) #-}
{-# ANN module ("HLint: ignore Monad law, left identity" :: String) #-}
{-# ANN module ("HLint: ignore Monad law, right identity" :: String) #-}
{-# ANN module ("HLint: ignore Use >=>" :: String) #-}
