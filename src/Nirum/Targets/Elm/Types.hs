{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.Elm.Types
    ( compileType
    , encoderName
    , encoderName'
    , typeName
    , typeName'
    ) where

import Control.Monad
import GHC.Exts (IsList (..))

import Data.Text (Text, append)
import Text.Blaze (Markup)
import Text.Heterocephalus (compileText)

import Nirum.Constructs.Identifier
import Nirum.Constructs.Name
import Nirum.Constructs.TypeDeclaration
import {-# SOURCE #-} Nirum.Targets.Elm ()
import Nirum.Targets.Elm.CodeGen
import Nirum.Targets.Elm.Encoder
import Nirum.Targets.Elm.TypeExpression
import Nirum.TypeInstance.BoundModule

typeName :: TypeDeclaration -> Text
typeName = typeName' . facialName . typename

typeName' :: Identifier -> Text
typeName' =
    -- CHECK: If a naming rule is changed update *Translation* chapter in
    -- *docs/target/elm.md* as well.
    toPascalCaseText

encoderName :: TypeDeclaration -> Text
encoderName = encoderName' . facialName . typename

encoderName' :: Identifier -> Text
encoderName' =
    -- CHECK: If a naming rule is changed update *Translation* chapter in
    -- *docs/target/elm.md* as well.
    (`append` "Encoder") . toCamelCaseText

compileType :: BoundModule Elm -> TypeDeclaration -> CodeGen Markup

compileType boundModule td@TypeDeclaration { type' = Alias canon } = do
    typeExpr <- compileTypeExpression boundModule canon
    mJEncode <- import' "Json.Encode"
    encoder <- compileEncoder boundModule canon
    return [compileText|
type alias #{typeName td} = #{typeExpr}

{-| The JSON encoder (serializer) of `#{typeName td}`.
-}
#{encoderName td} : #{typeName td} -> #{mJEncode}.Value
#{encoderName td} = #{encoder}
|]

compileType _ td@TypeDeclaration { type' = EnumType members' } = do
    mJEncode <- import' "Json.Encode"
    return [compileText|
type #{typeName td}
%{ forall (i, EnumMember memberName _) <- enumerate (toList members') }
%{ if i < 1 }
    = #{toPascalCaseText $ facialName memberName}
%{ else }
    | #{toPascalCaseText $ facialName memberName}
%{ endif }
%{ endforall }

{-| The JSON encoder (serializer) of `#{typeName td}`.
-}
#{encoderName td} : #{typeName td} -> #{mJEncode}.Value
#{encoderName td} value =
    (#{mJEncode}.string) <|
        case value of
%{ forall (EnumMember (Name f b) _) <- toList members' }
            #{toPascalCaseText f} -> "#{toSnakeCaseText b}"
%{ endforall }
|]

compileType boundModule
            td@TypeDeclaration { type' = UnboxedType innerType' } = do
    typeExpr <- compileTypeExpression boundModule innerType'
    mJEncode <- import' "Json.Encode"
    encoder <- compileEncoder boundModule innerType'
    return [compileText|
type #{typeName td}
    = #{typeName td} (#{typeExpr})

{-| The JSON encoder (serializer) of `#{typeName td}`.
-}
#{encoderName td} : #{typeName td} -> #{mJEncode}.Value
#{encoderName td} (#{typeName td} innerValue_) =
    let
        encode_ = (#{encoder})
    in
        encode_ innerValue_
|]

compileType boundModule
            td@TypeDeclaration { type' = RecordType fields'
                               , typename = Name { behindName = behindTypename }
                               } = do
    let fieldList = toList fields'
    fieldTypeExprs <- mapM (compileTypeExpression boundModule . fieldType)
                           fieldList
    fieldEncoders <- mapM (compileEncoder boundModule . fieldType)
                           fieldList
    let fieldNames = fmap (facialName . fieldName) fieldList
    mJEncode <- import' "Json.Encode"
    return [compileText|
type #{typeName td}
    = #{typeName td}
%{ forall (i, (fName, fTypeExpr)) <- enumerate (zip fieldNames fieldTypeExprs) }
%{ if i < 1 }
        { #{toCamelCaseText fName} : #{fTypeExpr}
%{ else }
        , #{toCamelCaseText fName} : #{fTypeExpr}
%{ endif }
%{ endforall }
        }

{-| The JSON encoder (serializer) of `#{typeName td}`.
-}
#{encoderName td} : #{typeName td} -> #{mJEncode}.Value
#{encoderName td} (#{typeName td} record_) =
    let
%{ forall (fName, fEncoder) <- zip fieldNames fieldEncoders }
        #{toCamelCaseText fName}_e = (#{fEncoder})
%{ endforall }
    in
        [ ("_type", #{mJEncode}.string "#{toSnakeCaseText behindTypename}")
%{ forall (Field (Name fName bName) _ _) <- fieldList }
        , ( "#{toSnakeCaseText bName}"
          , #{toCamelCaseText fName}_e record_.#{toCamelCaseText fName}
          )
%{ endforall }
        ] |> #{mJEncode}.object
|]

compileType boundModule
            td@TypeDeclaration { type' = union@UnionType {}
                               , typename = Name { behindName = behindTypename }
                               } = do
    tags' <- forM (toList $ tags union) $ \ (Tag tName fields' _) -> do
        let fields'' = toList fields'
        fieldTypeExprs <- mapM (compileTypeExpression boundModule . fieldType)
                               fields''
        fieldEncoders <- mapM (compileEncoder boundModule . fieldType)
                               fields''
        let fieldNames = fmap fieldName fields''
        return (tName, zip3 fieldNames fieldTypeExprs fieldEncoders)
    mJEncode <- import' "Json.Encode"
    return [compileText|
type #{typeName td}
%{ forall (i, (Name tagName _, fields')) <- enumerate tags' }
%{ if i < 1 }
    = #{toPascalCaseText tagName}
%{ else }
    | #{toPascalCaseText tagName}
%{ endif }
%{ if not (null fields') }
%{ forall (j, (Name fName _, fTypeExpr, _)) <- enumerate fields' }
%{ if j < 1 }
        { #{toCamelCaseText fName} : #{fTypeExpr}
%{ else }
        , #{toCamelCaseText fName} : #{fTypeExpr}
%{ endif }
%{ endforall }
        }
%{ endif }
%{ endforall }

{-| The JSON encoder (serializer) of `#{typeName td}`.
-}
#{encoderName td} : #{typeName td} -> #{mJEncode}.Value
#{encoderName td} value_ =
    case value_ of
%{ forall (Name tFName tBName, fields') <- tags' }
%{ if null fields' }
        #{toPascalCaseText tFName} ->
            [ ("_type", #{mJEncode}.string "#{toSnakeCaseText behindTypename}")
            , ("_tag", #{mJEncode}.string "#{toSnakeCaseText tBName}")
            ] |> #{mJEncode}.object
%{ else }
        #{toPascalCaseText tFName} r_ ->
            let
%{ forall (Name fName _, _, fEncoder) <- fields' }
                #{toCamelCaseText fName}_e = (#{fEncoder})
%{ endforall }
            in
                [ ( "_type"
                  , #{mJEncode}.string "#{toSnakeCaseText behindTypename}"
                  )
                , ("_tag", #{mJEncode}.string "#{toSnakeCaseText tBName}")
%{ forall (Name fName bName, _, _) <- fields' }
                , ( "#{toSnakeCaseText bName}"
                  , #{toCamelCaseText fName}_e r_.#{toCamelCaseText fName}
                  )
%{ endforall }
                ] |> #{mJEncode}.object
%{ endif }
%{ endforall }
|]

compileType _ TypeDeclaration { type' = PrimitiveType {} } =
    -- Must never happen.
    error "unexpected error during trying to compile a primitive ttype"

compileType _ ServiceDeclaration {} =
    return [compileText||]

compileType _ Import {} =
    return [compileText||]
