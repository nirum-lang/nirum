{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.Python.Validators
    ( Validator (..)
    , ValueValidator (..)
    , compilePrimitiveTypeValidator
    , compileValidator
    ) where

import Data.Text (Text, intercalate)
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs.Identifier
import Nirum.Constructs.TypeDeclaration
import Nirum.Constructs.TypeExpression
import {-# SOURCE #-} Nirum.Targets.Python ()
import Nirum.Targets.Python.CodeGen
import Nirum.Targets.Python.TypeExpression
import Nirum.TypeInstance.BoundModule

data Validator = Validator
    { typePredicateCode :: Code
    , valueValidators :: [ValueValidator]
    } deriving (Eq, Show)

data ValueValidator = ValueValidator
    { predicateCode :: Code
    , errorMessage :: Text
    } deriving (Eq, Show)

compileValidator :: BoundModule Python
                 -> TypeExpression
                 -> Code
                 -> CodeGen Validator
compileValidator mod' (OptionModifier typeExpr) pythonVar = do
    Validator typePred vvs <- compileValidator mod' typeExpr pythonVar
    let typeValidator = [qq|(($pythonVar) is None or $typePred)|]
        valueValidators' =
            [ ValueValidator [qq|(($pythonVar) is None or ($vPredCode))|] msg
            | ValueValidator vPredCode msg <- vvs
            ]
    return $ Validator typeValidator valueValidators'
compileValidator mod' (SetModifier typeExpr) pythonVar = do
    abc <- collectionsAbc
    Validator typePred vvs <-
        multiplexValidators mod' pythonVar [(typeExpr, "elem")]
    return $ Validator
        [qq|(isinstance($pythonVar, $abc.Set) and $typePred)|]
        vvs
compileValidator mod' (ListModifier typeExpr) pythonVar = do
    abc <- collectionsAbc
    Validator typePred vvs <-
        multiplexValidators mod' pythonVar [(typeExpr, "item")]
    return $ Validator
        [qq|(isinstance($pythonVar, $abc.Sequence) and $typePred)|]
        vvs
compileValidator mod' (MapModifier keyTypeExpr valueTypeExpr) pythonVar = do
    abc <- collectionsAbc
    Validator typePred vvs <-
        multiplexValidators mod' [qq|(($pythonVar).items())|]
        [(keyTypeExpr, "key"), (valueTypeExpr, "value")]
    return $ Validator
        [qq|(isinstance($pythonVar, $abc.Mapping) and $typePred)|]
        vvs
compileValidator mod' (TypeIdentifier typeId) pythonVar =
    case lookupType typeId mod' of
        Missing -> return $ Validator "False" []  -- must never happen
        Local (Alias typeExpr') -> compileValidator mod' typeExpr' pythonVar
        Imported modulePath' (Alias typeExpr') ->
            case resolveBoundModule modulePath' (boundPackage mod') of
                Nothing -> return $ Validator "False" []  -- must never happen
                Just foundMod -> compileValidator foundMod typeExpr' pythonVar
        Local PrimitiveType { primitiveTypeIdentifier = pId } ->
            compilePrimitiveTypeValidator pId pythonVar
        Imported _ PrimitiveType { primitiveTypeIdentifier = pId } ->
            compilePrimitiveTypeValidator pId pythonVar
        _ ->
            compileInstanceValidator mod' typeId pythonVar

compilePrimitiveTypeValidator :: PrimitiveTypeIdentifier
                              -> Code
                              -> CodeGen Validator
compilePrimitiveTypeValidator primitiveTypeId pythonVar = do
    typeName <- compilePrimitiveType primitiveTypeId
    return $ Validator
        [qq|(isinstance(($pythonVar), ($typeName)))|]
        (vv primitiveTypeId pythonVar)
  where
    vv :: PrimitiveTypeIdentifier -> Code -> [ValueValidator]
    vv Int32 var =
        [ ValueValidator [qq|(-0x80000000 <= ($var) < 0x80000000)|]
                         "out of range of 32-bit integer"
        ]
    vv Int64 var =
        [ ValueValidator
              [qq|(-0x8000000000000000 <= ($var) < 0x8000000000000000)|]
              "out of range of 64-bit integer"
        ]
    vv Datetime var =
        [ ValueValidator [qq|(($var).tzinfo is not None)|]
                         "naive datetime (lacking tzinfo)"
        ]
    vv Uri var =
        [ ValueValidator [qq|('\\n' not in ($var))|]
                         "URI cannot contain new line characters"
        ]
    vv _ _ = []

compileInstanceValidator :: BoundModule Python
                         -> Identifier
                         -> Code
                         -> CodeGen Validator
compileInstanceValidator mod' typeId pythonVar = do
    cls <- compileTypeExpression mod' (Just (TypeIdentifier typeId))
    return $ Validator [qq|(isinstance(($pythonVar), ($cls)))|] []

collectionsAbc :: CodeGen Code
collectionsAbc = do
    ver <- getPythonVersion
    importStandardLibrary $ case ver of
        Python2 -> "collections"
        Python3 -> "collections.abc"

multiplexValidators :: BoundModule Python
                    -> Code
                    -> [(TypeExpression, Code)]
                    -> CodeGen Validator
multiplexValidators mod' iterableExpr elements = do
    validators <- sequence
        [ do
              v <- compileValidator mod' tExpr elemVar
              return (elemVar, v)
        | (tExpr, var) <- elements
        , elemVar <- [mangleVar iterableExpr var]
        ]
    let csElemVars = intercalate "," [v | (v, _) <- validators]
        typePredLogicalAnds = intercalate
            " and "
            [typePred | (_, Validator typePred _) <- validators]
    return $ Validator
        [qq|(all(($typePredLogicalAnds) for ($csElemVars) in $iterableExpr))|]
        [ ValueValidator
              [qq|(all(($typePred) for ($csElemVars) in $iterableExpr))|]
              [qq|invalid elements ($msg)|]
        | (_, Validator _ vvs) <- validators
        , ValueValidator typePred msg <- vvs
        ]
