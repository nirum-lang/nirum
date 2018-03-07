{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.Python.TypeExpressionSpec where

import Test.Hspec.Meta
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs.Module
import Nirum.Constructs.TypeDeclaration
import Nirum.Constructs.TypeExpression
import Nirum.Targets.Python (Source (..))
import Nirum.Targets.Python.CodeGen
import Nirum.Targets.Python.CodeGenSpec hiding (spec)
import Nirum.Targets.Python.TypeExpression

spec :: Spec
spec = pythonVersionSpecs $ \ ver -> do
    let empty' = empty ver
        -- run' :: CodeGen a -> (Either CompileError a, CodeGenContext)
        run' c = runCodeGen c empty'
        -- code :: CodeGen a -> a
        code = either (const undefined) id . fst . run'

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
        let Source { sourceModule = bm } = makeDummySource $ Module [] Nothing
        specify "TypeIdentifier" $ do
            let (c, ctx) = run' $
                    compileTypeExpression bm (Just $ TypeIdentifier "bigint")
            standardImports ctx `shouldBe` []
            localImports ctx `shouldBe` []
            c `shouldBe` Right "int"
        specify "OptionModifier" $ do
            let (c', ctx') = run' $
                    compileTypeExpression bm (Just $ OptionModifier "int32")
            standardImports ctx' `shouldBe` ["typing"]
            localImports ctx' `shouldBe` []
            c' `shouldBe` Right "typing.Optional[int]"
        specify "SetModifier" $ do
            let (c'', ctx'') = run' $
                    compileTypeExpression bm (Just $ SetModifier "int32")
            standardImports ctx'' `shouldBe` ["typing"]
            localImports ctx'' `shouldBe` []
            c'' `shouldBe` Right "typing.AbstractSet[int]"
        specify "ListModifier" $ do
            let (c''', ctx''') = run' $
                    compileTypeExpression bm (Just $ ListModifier "int32")
            standardImports ctx''' `shouldBe` ["typing"]
            localImports ctx''' `shouldBe` []
            c''' `shouldBe` Right "typing.Sequence[int]"
        specify "MapModifier" $ do
            let (c'''', ctx'''') = run' $
                    compileTypeExpression bm (Just $ MapModifier "uuid" "int32")
            standardImports ctx'''' `shouldBe` ["typing", "uuid"]
            localImports ctx'''' `shouldBe` []
            c'''' `shouldBe` Right "typing.Mapping[uuid.UUID, int]"
