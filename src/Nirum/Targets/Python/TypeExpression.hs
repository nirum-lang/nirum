{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.Python.TypeExpression
    ( compileTypeExpression
    , compilePrimitiveType
    ) where

import Data.Text
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs.Identifier
import Nirum.Constructs.TypeDeclaration
import Nirum.Constructs.TypeExpression
import Nirum.Package.Metadata
import {-# SOURCE #-} Nirum.Targets.Python ()
import Nirum.Targets.Python.CodeGen
import Nirum.TypeInstance.BoundModule

compileTypeExpression :: BoundModule Python
                      -> Maybe TypeExpression
                      -> CodeGen Code
compileTypeExpression mod' (Just (TypeIdentifier i)) =
    case lookupType i mod' of
        Missing -> fail $ "undefined identifier: " ++ toString i
        Imported _ _ (PrimitiveType p _) -> compilePrimitiveType p
        Imported m in' _ -> do
            insertThirdPartyImportsA [ ( toImportPath target' m
                                       , [(toClassName i, toClassName in')]
                                       )
                                     ]
            return $ toClassName i
        Local _ -> return $ toClassName i
  where
    target' :: Python
    target' = target $ metadata $ boundPackage mod'
compileTypeExpression mod' (Just (MapModifier k v)) = do
    kExpr <- compileTypeExpression mod' (Just k)
    vExpr <- compileTypeExpression mod' (Just v)
    typing <- importStandardLibrary "typing"
    return [qq|$typing.Mapping[$kExpr, $vExpr]|]
compileTypeExpression mod' (Just modifier) = do
    expr <- compileTypeExpression mod' (Just typeExpr)
    typing <- importStandardLibrary "typing"
    return [qq|$typing.$className[$expr]|]
  where
    typeExpr :: TypeExpression
    className :: Text
    (typeExpr, className) = case modifier of
        OptionModifier t' -> (t', "Optional")
        SetModifier t' -> (t', "AbstractSet")
        ListModifier t' -> (t', "Sequence")
        TypeIdentifier _ -> undefined  -- never happen!
        MapModifier _ _ -> undefined  -- never happen!
compileTypeExpression _ Nothing =
    return "None"

compilePrimitiveType :: PrimitiveTypeIdentifier -> CodeGen Code
compilePrimitiveType primitiveTypeIdentifier' = do
    pyVer <- getPythonVersion
    case (primitiveTypeIdentifier', pyVer) of
        (Bool, _) -> builtins "bool"
        (Bigint, Python2) -> do
            numbers <- importStandardLibrary "numbers"
            return [qq|$numbers.Integral|]
        (Bigint, Python3) -> builtins "int"
        (Decimal, _) -> do
            decimal <- importStandardLibrary "decimal"
            return [qq|$decimal.Decimal|]
        (Int32, _) -> compilePrimitiveType Bigint
        (Int64, _) -> compilePrimitiveType Bigint
        (Float32, _) -> builtins "float"
        (Float64, _) -> builtins "float"
        (Text, Python2) -> builtins "unicode"
        (Text, Python3) -> builtins "str"
        (Binary, _) -> builtins "bytes"
        (Date, _) -> do
            datetime <- importStandardLibrary "datetime"
            return [qq|$datetime.date|]
        (Datetime, _) -> do
            datetime <- importStandardLibrary "datetime"
            return [qq|$datetime.datetime|]
        (Uuid, _) -> do
            uuid <- importStandardLibrary "uuid"
            return [qq|$uuid.UUID|]
        (Uri, Python2) -> builtins "basestring"
        (Uri, Python3) -> builtins "str"
  where
    builtins :: Code -> CodeGen Code
    builtins typename' = do
        builtinsMod <- importBuiltins
        return [qq|$builtinsMod.$typename'|]
