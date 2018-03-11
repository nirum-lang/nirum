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
        builtinsPair =
            ( "__builtin__"
            , case ver of
                  Python2 -> "__builtin__"
                  Python3 -> "builtins"
            )

    specify [qq|compilePrimitiveType ($ver)|] $ do
        let (boolCode, boolContext) = run' $ compilePrimitiveType Bool
        boolCode `shouldBe` Right "__builtin__.bool"
        standardImports boolContext `shouldBe` [builtinsPair]
        code (compilePrimitiveType Bigint) `shouldBe` "__builtin__.int"
        let (decimalCode, decimalContext) = run' (compilePrimitiveType Decimal)
        decimalCode `shouldBe` Right "_decimal.Decimal"
        standardImports decimalContext `shouldBe` [("_decimal", "decimal")]
        code (compilePrimitiveType Int32) `shouldBe` "__builtin__.int"
        code (compilePrimitiveType Int64) `shouldBe`
            case ver of
                Python2 -> "_numbers.Integral"
                Python3 -> "__builtin__.int"
        code (compilePrimitiveType Float32) `shouldBe` "__builtin__.float"
        code (compilePrimitiveType Float64) `shouldBe` "__builtin__.float"
        code (compilePrimitiveType Text) `shouldBe`
            case ver of
                Python2 -> "__builtin__.unicode"
                Python3 -> "__builtin__.str"
        code (compilePrimitiveType Binary) `shouldBe` "__builtin__.bytes"
        let (dateCode, dateContext) = run' (compilePrimitiveType Date)
        dateCode `shouldBe` Right "_datetime.date"
        standardImports dateContext `shouldBe` [("_datetime", "datetime")]
        let (datetimeCode, datetimeContext) =
                run' (compilePrimitiveType Datetime)
        datetimeCode `shouldBe` Right "_datetime.datetime"
        standardImports datetimeContext `shouldBe` [("_datetime", "datetime")]
        let (uuidCode, uuidContext) = run' (compilePrimitiveType Uuid)
        uuidCode `shouldBe` Right "_uuid.UUID"
        standardImports uuidContext `shouldBe` [("_uuid", "uuid")]
        code (compilePrimitiveType Uri) `shouldBe`
            case ver of
                Python2 -> "__builtin__.basestring"
                Python3 -> "__builtin__.str"

    describe [qq|compileTypeExpression ($ver)|] $ do
        let Source { sourceModule = bm } = makeDummySource $ Module [] Nothing
        specify "TypeIdentifier" $ do
            let (c, ctx) = run' $
                    compileTypeExpression bm (Just $ TypeIdentifier "bigint")
            standardImports ctx `shouldBe` [builtinsPair]
            localImports ctx `shouldBe` []
            c `shouldBe` Right "__builtin__.int"
        specify "OptionModifier" $ do
            let (c', ctx') = run' $
                    compileTypeExpression bm (Just $ OptionModifier "binary")
            standardImports ctx' `shouldBe`
                [builtinsPair, ("_typing", "typing")]
            localImports ctx' `shouldBe` []
            c' `shouldBe` Right "_typing.Optional[__builtin__.bytes]"
        specify "SetModifier" $ do
            let (c'', ctx'') = run' $
                    compileTypeExpression bm (Just $ SetModifier "int32")
            standardImports ctx'' `shouldBe`
                [builtinsPair, ("_typing", "typing")]
            localImports ctx'' `shouldBe` []
            c'' `shouldBe` Right "_typing.AbstractSet[__builtin__.int]"
        specify "ListModifier" $ do
            let (c''', ctx''') = run' $
                    compileTypeExpression bm (Just $ ListModifier "int32")
            standardImports ctx''' `shouldBe`
                [builtinsPair, ("_typing", "typing")]
            localImports ctx''' `shouldBe` []
            c''' `shouldBe` Right "_typing.Sequence[__builtin__.int]"
        specify "MapModifier" $ do
            let (c'''', ctx'''') = run' $
                    compileTypeExpression bm (Just $ MapModifier "uuid" "int32")
            standardImports ctx'''' `shouldBe`
                [builtinsPair, ("_typing", "typing"), ("_uuid", "uuid")]
            localImports ctx'''' `shouldBe` []
            c'''' `shouldBe`
                Right "_typing.Mapping[_uuid.UUID, __builtin__.int]"
