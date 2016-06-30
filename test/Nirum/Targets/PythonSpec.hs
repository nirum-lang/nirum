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

import Control.Monad (void, unless)
import Data.Char (isSpace)

import Data.List (dropWhileEnd)
import qualified Data.Text as T
import System.Info (os)
import System.Process (readProcess)
import Test.Hspec.Meta
import Text.InterpolatedString.Perl6 (q, qq)
import Text.Megaparsec (char, digitChar, runParser, some, space, string')
import Text.Megaparsec.String (Parser)

import Nirum.Constructs.DeclarationSet (DeclarationSet)
import Nirum.Constructs.Module (Module(Module))
import Nirum.Constructs.Name (Name(Name))
import Nirum.Constructs.TypeDeclaration ( Field(Field)
                                        , EnumMember(EnumMember)
                                        , Tag(Tag)
                                        , Type(BoxedType, EnumType, RecordType,
                                               UnionType)
                                        , TypeDeclaration(TypeDeclaration)
                                        )
import Nirum.Constructs.TypeExpression ( TypeExpression( ListModifier
                                                       , MapModifier
                                                       , OptionModifier
                                                       , SetModifier
                                                       , TypeIdentifier
                                                       )
                                       )
import Nirum.Targets.Python ( CodeGen( code
                                     , localImports
                                     , packages
                                     , standardImports
                                     , thirdPartyImports
                                     )
                            , compileModule
                            , compileTypeExpression
                            , toAttributeName
                            , toClassName
                            , withLocalImport
                            , withPackage
                            , withStandardImport
                            , withThirdPartyImports
                            )

codeGen :: a -> CodeGen a
codeGen = return

windows :: Bool
windows = os `elem` (["mingw32", "cygwin32", "win32"] :: [String])

data PyVersion = PyVersion Int Int Int deriving (Eq, Ord, Show)

isPythonInstalled :: IO Bool
isPythonInstalled = do
    pyExist <- readProcess which ["python3"] ""
    return $ not $ all isSpace pyExist
  where
    which :: String
    which = if windows then "where" else "which"

getPythonVersion :: IO (Maybe PyVersion)
getPythonVersion = do
    installed <- isPythonInstalled
    if installed then do
        pyVersionStr <- readProcess "python3" ["-V"] ""
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

runPython :: String -> IO (Maybe String)
runPython code' = do
    pyVerM <- getPythonVersion
    case pyVerM of
        Nothing -> do
            putStrLn "Can't determine Python version; skipping..."
            return Nothing
        Just version ->
            if version < PyVersion 3 3 0 then do
                putStrLn "Python seems below 3.3; skipping..."
                return Nothing
            else do
                result <- readProcess "python3" [] code'
                return $ Just result

testPythonSuit :: String -> T.Text -> IO ()
testPythonSuit suitCode testCode = do
    nirumPackageInstalledM <-
        runPython [q|
try: import nirum
except ImportError: print('F')
else: print('T')
            |]
    case nirumPackageInstalledM of
        Just nirumPackageInstalled ->
            case strip nirumPackageInstalled of
                "T" -> do
                    resultM <- runPython suitCode
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

testPython :: T.Text -> T.Text -> IO ()
testPython defCode testCode = testPythonSuit code' testCode'
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

testRaisePython :: T.Text -> T.Text -> T.Text -> IO ()
testRaisePython defCode errorClassName testCode =
    testPythonSuit code' testCode''
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
                        b <- withPackage "nirum" cg
                        c' <- withThirdPartyImports
                            [("nirum", ["serialize_boxed_type"])]
                            cg
                        d <- withLocalImport ".." "Gender" cg
                        e <- withStandardImport "os" cg
                        f'' <- withPackage "nirum" cg
                        g'' <- withThirdPartyImports
                            [("nirum", ["serialize_enum_type"])]
                            cg
                        h'' <- withLocalImport ".." "Path" cg
                        return $ sum ([a, b, c', d, e, f'', g'', h''] :: [Int])
                packages c `shouldBe` ["nirum"]
                standardImports c `shouldBe` ["os", "sys"]
                thirdPartyImports c `shouldBe`
                    [("nirum", ["serialize_boxed_type", "serialize_enum_type"])]
                localImports c `shouldBe` [("..", ["Gender", "Path"])]
                code c `shouldBe` (123 * 8)
        specify "withStandardImport" $ do
            let codeGen1 = withStandardImport "sys" (pure True)
            packages codeGen1 `shouldBe` []
            standardImports codeGen1 `shouldBe` ["sys"]
            thirdPartyImports codeGen1 `shouldBe` []
            localImports codeGen1 `shouldBe` []
            code codeGen1 `shouldBe` True
            let codeGen2 = withStandardImport "os" codeGen1
            packages codeGen2 `shouldBe` []
            standardImports codeGen2 `shouldBe` ["os", "sys"]
            thirdPartyImports codeGen2 `shouldBe` []
            localImports codeGen2 `shouldBe` []
            code codeGen2 `shouldBe` True

    describe "compileTypeExpression" $ do
        specify "TypeIdentifier" $ do
            let c = compileTypeExpression (TypeIdentifier "bigint")
            packages c `shouldBe` []
            standardImports c `shouldBe` []
            localImports c `shouldBe` []
            code c `shouldBe` "Bigint"  -- FIXME: numbers.Integral
        specify "OptionModifier" $ do
            let c' = compileTypeExpression (OptionModifier "text")
            packages c' `shouldBe` []
            standardImports c' `shouldBe` ["typing"]
            localImports c' `shouldBe` []
            code c' `shouldBe` "typing.Optional[Text]"
            -- FIXME: typing.Optional[str]
        specify "SetModifier" $ do
            let c'' = compileTypeExpression (SetModifier "text")
            packages c'' `shouldBe` []
            standardImports c'' `shouldBe` ["typing"]
            localImports c'' `shouldBe` []
            code c'' `shouldBe` "typing.AbstractSet[Text]"
            -- FIXME: typing.AbstractSet[str]
        specify "ListModifier" $ do
            let c''' = compileTypeExpression (ListModifier "text")
            packages c''' `shouldBe` []
            standardImports c''' `shouldBe` ["typing"]
            localImports c''' `shouldBe` []
            code c''' `shouldBe` "typing.Sequence[Text]"
            -- FIXME: typing.Sequence[str]
        specify "MapModifier" $ do
            let c'''' = compileTypeExpression (MapModifier "uuid" "text")
            packages c'''' `shouldBe` []
            standardImports c'''' `shouldBe` ["typing"]
            localImports c'''' `shouldBe` []
            code c'''' `shouldBe` "typing.Mapping[Uuid, Text]"
            -- FIXME: typing.Mapping[uuid.UUID, str]

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

    describe "compileModule" $ do
        let tM module' = testPython $ compileModule module'
            tT typeDecl = tM $ Module [typeDecl] Nothing
            tR module' = testRaisePython $ compileModule module'
            tR' typeDecl = tR $ Module [typeDecl] Nothing
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


{-# ANN module ("HLint: ignore Functor law" :: String) #-}
{-# ANN module ("HLint: ignore Monad law, left identity" :: String) #-}
{-# ANN module ("HLint: ignore Monad law, right identity" :: String) #-}
{-# ANN module ("HLint: ignore Use >=>" :: String) #-}
