{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.Elm.Encoder (compileEncoder) where

import Data.Text (Text)
import Text.Blaze (Markup)
import Text.Heterocephalus (compileText)

import Nirum.Constructs.Identifier
import Nirum.Constructs.TypeDeclaration
import Nirum.Constructs.TypeExpression
import {-# SOURCE #-} Nirum.Targets.Elm ()
import Nirum.Targets.Elm.CodeGen
import Nirum.TypeInstance.BoundModule

compileEncoder :: BoundModule Elm -> TypeExpression -> CodeGen Markup

compileEncoder boundModule (OptionModifier valType) = do
    mMaybe <- import' "Maybe"
    mJEncode <- import' "Json.Encode"
    valEncoder <- compileEncoder boundModule valType
    return [compileText|#{mMaybe}.map (#{valEncoder})
        >> #{mMaybe}.withDefault #{mJEncode}.null|]

compileEncoder boundModule (SetModifier elemType) = do
    mSet <- import' "Set"
    mJEncode <- import' "Json.Encode"
    elemEncoder <- compileEncoder boundModule elemType
    return [compileText|#{mSet}.map (#{elemEncoder})
        >> #{mSet}.toList
        >> #{mJEncode}.list|]

compileEncoder boundModule (ListModifier elemType) = do
    mList <- import' "List"
    mJEncode <- import' "Json.Encode"
    elemEncoder <- compileEncoder boundModule elemType
    return [compileText|#{mList}.map (#{elemEncoder}) >> #{mJEncode}.list|]

compileEncoder boundModule (MapModifier kType vType) = do
    mDict <- import' "Dict"
    mList <- import' "List"
    mTuple <- import' "Tuple"
    mJEncode <- import' "Json.Encode"
    kEncoder <- compileEncoder boundModule kType
    vEncoder <- compileEncoder boundModule vType
    return [compileText|#{mDict}.toList
        >> #{mList}.map (#{mTuple}.mapFirst (#{kEncoder})
            >> #{mTuple}.mapSecond (#{vEncoder})
            >> (\(k_, v_) -> [("key", k_), ("value", v_)])
            >> #{mJEncode}.object)
        >> #{mJEncode}.list|]

compileEncoder boundModule (TypeIdentifier typeId) =
    case lookupType typeId boundModule of
        Missing ->  -- must never happen
            fail $ "failed to resolve the type " ++ show typeId
        Imported _ (PrimitiveType primitiveTypeId _) ->
            compilePrimitiveTypeEncoder primitiveTypeId
        Imported modulePath' _ -> do
            mod' <- import' (toImportPath modulePath')
            return [compileText|#{mod'}.#{toCamelCaseText typeId}Encoder|]
        Local (PrimitiveType primitiveTypeId _) ->
            compilePrimitiveTypeEncoder primitiveTypeId
        Local _ ->
            return [compileText|#{toCamelCaseText typeId}Encoder|]

compilePrimitiveTypeEncoder :: PrimitiveTypeIdentifier -> CodeGen Markup
compilePrimitiveTypeEncoder primitiveTypeId = case primitiveTypeId of
    Bigint -> jsonEncode "int"
    Decimal -> do
        require ElmDecimal
        mDecimal <- import' "Decimal"
        mJEncode <- import' "Json.Encode"
        return [compileText|#{mDecimal}.toString >> #{mJEncode}.string|]
    Int32 -> jsonEncode "int"
    Int64 -> jsonEncode "int"
    Float32 -> jsonEncode "float"
    Float64 -> jsonEncode "float"
    Text -> jsonEncode "string"
    Binary -> do
        require ElmBytes
        require ElmBase64
        mBytes <- import' "Bytes"
        mBase64 <- import' "Base64"
        mJEncode <- import' "Json.Encode"
        return [compileText|#{mBytes}.toString
            >> #{mBase64}.encode
            >> #{mJEncode}.string|]
    Date -> do
        require ElmTime
        mTDate <- import' "Time.Date"
        mJEncode <- import' "Json.Encode"
        return [compileText|#{mTDate}.toISO8601 >> #{mJEncode}.string|]
    Datetime -> do
        require ElmTime
        mTZonedDateTime <- import' "Time.ZonedDateTime"
        mTTZ <- import' "Time.TimeZones"
        mJEncode <- import' "Json.Encode"
        return [compileText|#{mTZonedDateTime}.asTimeZone (#{mTTZ}.utc ())
            >> #{mTZonedDateTime}.toISO8601
            >> #{mJEncode}.string|]
    Bool -> jsonEncode "bool"
    Uuid -> do
        require ElmUuid
        mUuid <- import' "Uuid"
        mString <- import' "String"
        mJEncode <- import' "Json.Encode"
        -- Although elm-uuid provides a function:
        --   Uuid.encode : Uuid.Uuid -> Json.Encode.Value
        -- since a UUID is rendered as capital letters, we make our own encoder.
        return [compileText|#{mUuid}.toString
            >> #{mString}.toLower
            >> #{mJEncode}.string|]
    Uri -> jsonEncode "string"
  where
    jsonEncode :: Text -> CodeGen Markup
    jsonEncode encoderName = do
        mJsonEncode <- import' "Json.Encode"
        return [compileText|#{mJsonEncode}.#{encoderName}|]
