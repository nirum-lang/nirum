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
import Nirum.Constructs.TypeDeclaration ( EnumMember(EnumMember)
                                        , Type(BoxedType, EnumType)
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
                            , withLocalImport
                            , withPackage
                            , withStandardImport
                            , withThirdPartyImport
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

testPython :: T.Text -> T.Text -> IO ()
testPython defCode testCode = do
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
                    resultM <- runPython code'
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
    code' :: String
    code' = [qq|$defCode

if __name__ == '__main__':
    print(bool($testCode))
|]
    strip :: String -> String
    strip = dropWhile isSpace . dropWhileEnd isSpace

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
                        c' <- withThirdPartyImport "nirum"
                                                   "serialize_boxed_type" cg
                        d <- withLocalImport ".." "Gender" cg
                        e <- withStandardImport "os" cg
                        f'' <- withPackage "nirum" cg
                        g'' <- withThirdPartyImport "nirum"
                                                    "serialize_enum_type" cg
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
            code c `shouldBe` "bigint"  -- FIXME: numbers.Integral
        specify "OptionModifier" $ do
            let c' = compileTypeExpression (OptionModifier "text")
            packages c' `shouldBe` []
            standardImports c' `shouldBe` ["typing"]
            localImports c' `shouldBe` []
            code c' `shouldBe` "typing.Optional[text]"
            -- FIXME: typing.Optional[str]
        specify "SetModifier" $ do
            let c'' = compileTypeExpression (SetModifier "text")
            packages c'' `shouldBe` []
            standardImports c'' `shouldBe` ["typing"]
            localImports c'' `shouldBe` []
            code c'' `shouldBe` "typing.AbstractSet[text]"
            -- FIXME: typing.AbstractSet[str]
        specify "ListModifier" $ do
            let c''' = compileTypeExpression (ListModifier "text")
            packages c''' `shouldBe` []
            standardImports c''' `shouldBe` ["typing"]
            localImports c''' `shouldBe` []
            code c''' `shouldBe` "typing.Sequence[text]"
            -- FIXME: typing.Sequence[str]
        specify "MapModifier" $ do
            let c'''' = compileTypeExpression (MapModifier "uuid" "text")
            packages c'''' `shouldBe` []
            standardImports c'''' `shouldBe` ["typing"]
            localImports c'''' `shouldBe` []
            code c'''' `shouldBe` "typing.Mapping[uuid, text]"
            -- FIXME: typing.Mapping[uuid.UUID, str]

    describe "compileModule" $ do
        let tM module' = testPython $ compileModule module'
            tT typeDecl = tM $ Module [typeDecl] Nothing
        specify "boxed type" $ do
            let decl = TypeDeclaration "offset" (BoxedType "float64") Nothing
            tT decl "isinstance(offset, type)"
            tT decl "offset(3.14).value == 3.14"
            tT decl "offset(3.14) == offset(3.14)"
            tT decl "offset(3.14) != offset(1.0)"
            tT decl [q|{offset(3.14), offset(3.14), offset(1.0)} ==
                       {offset(3.14), offset(1.0)}|]
            tT decl "offset(3.14).__nirum_serialize__() == 3.14"
            tT decl "offset.__nirum_deserialize__(3.14) == offset(3.14)"
        specify "enum type" $ do
            let members = [ "male"
                          , EnumMember (Name "female" "yeoseong") Nothing
                          ] :: DeclarationSet EnumMember
                decl = TypeDeclaration "gender"
                                       (EnumType members)
                                       Nothing
            tT decl "type(gender) is enum.EnumMeta"
            tT decl "set(gender) == {gender.male, gender.female}"
            tT decl "gender.male.value == 'male'"
            tT decl "gender.female.value == 'yeoseong'"
            tT decl "gender.__nirum_deserialize__('male') == gender.male"
            tT decl "gender.__nirum_deserialize__('yeoseong') == gender.female"
            -- TODO: test deserializer with invalid input
            tT decl "gender.male.__nirum_serialize__() == 'male'"
            tT decl "gender.female.__nirum_serialize__() == 'yeoseong'"

{-# ANN module ("HLint: ignore Functor law" :: String) #-}
{-# ANN module ("HLint: ignore Monad law, left identity" :: String) #-}
{-# ANN module ("HLint: ignore Monad law, right identity" :: String) #-}
{-# ANN module ("HLint: ignore Use >=>" :: String) #-}
