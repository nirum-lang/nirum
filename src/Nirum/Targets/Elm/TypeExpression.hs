{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.Elm.TypeExpression (compileTypeExpression) where

import Data.Text (Text)
import Text.Blaze (Markup)
import Text.Heterocephalus (compileText)

import Nirum.Constructs.Identifier
import Nirum.Constructs.TypeDeclaration
import Nirum.Constructs.TypeExpression
import {-# SOURCE #-} Nirum.Targets.Elm ()
import Nirum.Targets.Elm.CodeGen
import Nirum.TypeInstance.BoundModule

compileTypeExpression :: BoundModule Elm -> TypeExpression -> CodeGen Markup

compileTypeExpression boundModule (OptionModifier valType) = do
    mMaybe <- import' "Maybe"
    valueTypeExpr <- compileTypeExpression boundModule valType
    return [compileText|(#{mMaybe}.Maybe (#{valueTypeExpr}))|]

compileTypeExpression boundModule (SetModifier elemType) = do
    mSet <- import' "Set"
    elemTypeExpr <- compileTypeExpression boundModule elemType
    return [compileText|(#{mSet}.Set (#{elemTypeExpr}))|]

compileTypeExpression boundModule (ListModifier elemType) = do
    require ElmNirum
    mPrimitives <- import' "Nirum.Primitives"
    elemTypeExpr <- compileTypeExpression boundModule elemType
    return [compileText|(#{mPrimitives}.List_ (#{elemTypeExpr}))|]

compileTypeExpression boundModule (MapModifier kType vType) = do
    mDict <- import' "Dict"
    kTypeExpr <- compileTypeExpression boundModule kType
    vTypeExpr <- compileTypeExpression boundModule vType
    return [compileText|(#{mDict}.Dict (#{kTypeExpr}, #{vTypeExpr}))|]

compileTypeExpression boundModule (TypeIdentifier typeId) =
    case lookupType typeId boundModule of
        Missing ->  -- must never happen
            fail $ "failed to resolve the type " ++ show typeId
        Imported _ (PrimitiveType primitiveTypeId _) ->
            compilePrimitiveType primitiveTypeId
        Imported modulePath' _ -> do
            mod' <- import' (toImportPath modulePath')
            return [compileText|#{mod'}.#{toPascalCaseText typeId}|]
        Local (PrimitiveType primitiveTypeId _) ->
            compilePrimitiveType primitiveTypeId
        Local _ ->
            return [compileText|#{toPascalCaseText typeId}|]

compilePrimitiveType :: PrimitiveTypeIdentifier -> CodeGen Markup
compilePrimitiveType primitiveTypeId = case primitiveTypeId of
    Bigint -> prim "Bigint"
    Decimal -> do
        require ElmDecimal
        mDecimal <- import' "Decimal"
        return [compileText|#{mDecimal}.Decimal|]
    Int32 -> prim "Int32"
    Int64 -> prim "Int64"
    Float32 -> prim "Float32"
    Float64 -> prim "Float64"
    Text -> prim "Text"
    Binary -> do
        -- CHECK: Find more appropriate library.
        require ElmBytes
        mBytes <- import' "Bytes"
        return [compileText|#{mBytes}.Bytes|]
    Date -> do
        require ElmTime
        mTDate <- import' "Time.Date"
        return [compileText|#{mTDate}.Date|]
    Datetime -> do
        require ElmTime
        mTZonedDateTime <- import' "Time.ZonedDateTime"
        return [compileText|#{mTZonedDateTime}.ZonedDateTime|]
    Bool -> prim "Bool_"
    Uuid -> do
        require ElmUuid
        mUuid <- import' "Uuid"
        return [compileText|#{mUuid}.Uuid|]
    Uri -> prim "Text"
  where
    prim :: Text -> CodeGen Markup
    prim typeName = do
        require ElmNirum
        mPrim <- import' "Nirum.Primitives"
        return [compileText|#{mPrim}.#{typeName}|]
