{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.Python.Serializers (compileSerializer) where

import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs.TypeDeclaration
import Nirum.Constructs.TypeExpression
import {-# SOURCE #-} Nirum.Targets.Python ()
import Nirum.Targets.Python.CodeGen
import Nirum.TypeInstance.BoundModule

compileSerializer :: BoundModule Python -> TypeExpression -> Code -> Code
compileSerializer mod' (OptionModifier typeExpr) pythonVar =
    [qq|(None if ($pythonVar) is None
              else ({compileSerializer mod' typeExpr pythonVar}))|]
compileSerializer mod' (SetModifier typeExpr) pythonVar =
    compileSerializer mod' (ListModifier typeExpr) pythonVar
compileSerializer mod' (ListModifier typeExpr) pythonVar =
    [qq|list(($serializer) for $elemVar in ($pythonVar))|]
  where
    elemVar :: Code
    elemVar = mangleVar pythonVar "elem"
    serializer :: Code
    serializer = compileSerializer mod' typeExpr elemVar
compileSerializer mod' (MapModifier kt vt) pythonVar =
    [qq|list(
        \{
            'key': ({compileSerializer mod' kt kVar}),
            'value': ({compileSerializer mod' vt vVar}),
        \}
        for $kVar, $vVar in ($pythonVar).items()
    )|]
  where
    kVar :: Code
    kVar = mangleVar pythonVar "key"
    vVar :: Code
    vVar = mangleVar pythonVar "value"
compileSerializer mod' (TypeIdentifier typeId) pythonVar =
    case lookupType typeId mod' of
        Missing -> "None"  -- must never happen
        Local (Alias t) -> compileSerializer mod' t pythonVar
        Imported modulePath' _ (Alias t) ->
            case resolveBoundModule modulePath' (boundPackage mod') of
                Nothing -> "None"  -- must never happen
                Just foundMod -> compileSerializer foundMod t pythonVar
        Local PrimitiveType { primitiveTypeIdentifier = p } ->
            compilePrimitiveTypeSerializer p pythonVar
        Imported _ _ PrimitiveType { primitiveTypeIdentifier = p } ->
            compilePrimitiveTypeSerializer p pythonVar
        Local EnumType {} -> serializerCall
        Imported _ _ EnumType {} -> serializerCall
        Local RecordType {} -> serializerCall
        Imported _ _ RecordType {} -> serializerCall
        Local UnboxedType {} -> serializerCall
        Imported _ _ UnboxedType {} -> serializerCall
        Local UnionType {} -> serializerCall
        Imported _ _ UnionType {} -> serializerCall
  where
    serializerCall :: Code
    serializerCall = [qq|$pythonVar.__nirum_serialize__()|]

compilePrimitiveTypeSerializer :: PrimitiveTypeIdentifier -> Code -> Code
compilePrimitiveTypeSerializer Bigint var = var
compilePrimitiveTypeSerializer Decimal var = [qq|str($var)|]
compilePrimitiveTypeSerializer Int32 var = var
compilePrimitiveTypeSerializer Int64 var = var
compilePrimitiveTypeSerializer Float32 var = var
compilePrimitiveTypeSerializer Float64 var = var
compilePrimitiveTypeSerializer Text var = var
compilePrimitiveTypeSerializer Binary var =
    [qq|__import__('base64').b64encode($var).decode('ascii')|]
compilePrimitiveTypeSerializer Date var = [qq|($var).isoformat()|]
compilePrimitiveTypeSerializer Datetime var = [qq|($var).isoformat()|]
compilePrimitiveTypeSerializer Bool var = var
compilePrimitiveTypeSerializer Uuid var = [qq|str($var).lower()|]
compilePrimitiveTypeSerializer Uri var = var
