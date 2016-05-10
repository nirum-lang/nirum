{-# LANGUAGE OverloadedLists, OverloadedStrings, ScopedTypeVariables #-}
module Nirum.Targets.PythonSpec where

import Test.Hspec.Meta

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
                            , compileTypeExpression
                            , withLocalImport
                            , withPackage
                            , withStandardImport
                            , withThirdPartyImport
                            )

codeGen :: a -> CodeGen a
codeGen = return

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

{-# ANN module ("HLint: ignore Functor law" :: String) #-}
{-# ANN module ("HLint: ignore Monad law, left identity" :: String) #-}
{-# ANN module ("HLint: ignore Monad law, right identity" :: String) #-}
{-# ANN module ("HLint: ignore Use >=>" :: String) #-}
