{-# LANGUAGE ExtendedDefaultRules, OverloadedLists, OverloadedStrings,
             QuasiQuotes #-}
module Nirum.Targets.Python ( Code
                            , CodeGen( code
                                     , localImports
                                     , packages
                                     , standardImports
                                     , thirdPartyImports
                                     )
                            , compileModule
                            , compileTypeDeclaration
                            , compileTypeExpression
                            , toAttributeName
                            , toClassName
                            , withLocalImport
                            , withPackage
                            , withStandardImport
                            , withThirdPartyImports
                            ) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs.DeclarationSet (DeclarationSet, toList)
import Nirum.Constructs.Identifier ( Identifier
                                   , toPascalCaseText
                                   , toSnakeCaseText
                                   )
import Nirum.Constructs.Module (Module(Module, types))
import Nirum.Constructs.Name (Name(Name))
import qualified Nirum.Constructs.Name as N
import Nirum.Constructs.TypeDeclaration ( EnumMember(EnumMember)
                                        , Field(Field)
                                        , Tag(Tag)
                                        , Type(Alias, BoxedType, EnumType,
                                               RecordType, UnionType)
                                        , TypeDeclaration(TypeDeclaration)
                                        )
import Nirum.Constructs.TypeExpression ( TypeExpression( ListModifier
                                                       , MapModifier
                                                       , OptionModifier
                                                       , SetModifier
                                                       , TypeIdentifier
                                                       )
                                       )

type Code = T.Text

data CodeGen a = CodeGen { packages :: S.Set T.Text
                         , standardImports :: S.Set T.Text
                         , thirdPartyImports :: M.Map T.Text (S.Set T.Text)
                         , localImports :: M.Map T.Text (S.Set T.Text)
                         , code :: a
                         } deriving (Eq, Ord, Show)

instance Functor CodeGen where
    fmap f codeGen = pure f <*> codeGen

instance Applicative CodeGen where
    pure = return
    c@CodeGen { code = f } <*> codeGen = codeGen >>= \x -> c { code = f x }

instance Monad CodeGen where
    return code' = CodeGen { packages = []
                           , standardImports = []
                           , thirdPartyImports = []
                           , localImports = []
                           , code = code'
                           }
    (CodeGen p si ti li c) >>= f =
        CodeGen packages' stdImports thirdPartyImports' localImports' code'
      where
        (CodeGen p' si' ti' li' code') = f c
        packages' = S.union p p'
        stdImports = S.union si si'
        thirdPartyImports' = M.unionWith S.union ti ti'
        localImports' = M.unionWith S.union li li'

withPackage :: T.Text -> CodeGen a -> CodeGen a
withPackage package c@CodeGen { packages = p } =
    c { packages = S.insert package p }

withStandardImport :: T.Text -> CodeGen a -> CodeGen a
withStandardImport module' c@CodeGen { standardImports = si } =
    c { standardImports = S.insert module' si }

withThirdPartyImports :: [(T.Text, S.Set T.Text)] -> CodeGen a -> CodeGen a
withThirdPartyImports imports c@CodeGen { thirdPartyImports = ti } =
    c { thirdPartyImports = L.foldl (M.unionWith S.union) ti importList }
  where
    importList :: [M.Map T.Text (S.Set T.Text)]
    importList = map (uncurry M.singleton) imports

withLocalImport :: T.Text -> T.Text -> CodeGen a -> CodeGen a
withLocalImport module' object c@CodeGen { localImports = li } =
    c { localImports = M.insertWith S.union module' [object] li }

-- | The set of Python reserved keywords.
-- See also: https://docs.python.org/3/reference/lexical_analysis.html#keywords
keywords :: S.Set T.Text
keywords = [ "False", "None", "True"
           , "and", "as", "assert", "break", "class", "continue"
           , "def", "del" , "elif", "else", "except", "finally"
           , "for", "from", "global", "if", "import", "in", "is"
           , "lambda", "nonlocal", "not", "or", "pass", "raise"
           , "return", "try", "while", "with", "yield"
           ]

toClassName :: Identifier -> T.Text
toClassName identifier =
    if className `S.member` keywords then className `T.snoc` '_' else className
  where
    className :: T.Text
    className = toPascalCaseText identifier

toClassName' :: Name -> T.Text
toClassName' = toClassName . N.facialName

toAttributeName :: Identifier -> T.Text
toAttributeName identifier =
    if attrName `S.member` keywords then attrName `T.snoc` '_' else attrName
  where
    attrName :: T.Text
    attrName = toSnakeCaseText identifier

toAttributeName' :: Name -> T.Text
toAttributeName' = toAttributeName . N.facialName

toIndentedCodes :: (a -> T.Text) -> [a] -> T.Text -> T.Text
toIndentedCodes f traversable concatenator =
    T.intercalate concatenator $ map f traversable

compileUnionTag :: Name -> Name -> DeclarationSet Field -> CodeGen Code
compileUnionTag parentname typename fields = do
    typeExprCodes <- mapM compileTypeExpression
        [typeExpr | (Field _ typeExpr _) <- toList fields]
    let className = toClassName' typename
        tagNames = map toAttributeName' [ name
                                        | (Field name _ _) <- toList fields
                                        ]
        nameNTypes = zip tagNames typeExprCodes
        slotTypes = toIndentedCodes
            (\(n, t) -> [qq|'{n}': {t}|]) nameNTypes ",\n        "
        slots = toIndentedCodes (\n -> [qq|'{n}'|]) tagNames ",\n        "
        initialArgs = toIndentedCodes
            (\(n, t) -> [qq|{n}: {t}|]) nameNTypes ", "
        initialValues =
            toIndentedCodes (\n -> [qq|self.{n} = {n}|]) tagNames "\n        "
        nameMaps = toIndentedCodes
            (\(Name f b) ->
                [qq|('{toAttributeName f}', '{toSnakeCaseText b}')|])
            [name | Field name _ _ <- toList fields, N.isComplex name]
            ",\n        "
        parentClass = toClassName' parentname
    withStandardImport "typing" $
        withThirdPartyImports
            [("nirum.validate", ["validate_union_type"])] $
            return [qq|
class $className($parentClass):
    # TODO: docstring

    __slots__ = (
        $slots,
    )
    __nirum_tag__ = $parentClass.Tag.{toAttributeName' typename}
    __nirum_tag_types__ = \{
        $slotTypes
    \}
    __nirum_tag_names__ = NameDict([
        $nameMaps
    ])

    def __init__(self, $initialArgs) -> None:
        $initialValues
        validate_union_type(self)

    def __repr__(self) -> str:
        return '\{0.__module__\}.\{0.__qualname__\}(\{1\})'.format(
            type(self),
            ', '.join('\{\}=\{\}'.format(attr, getattr(self, attr))
                      for attr in self.__slots__)
        )

    def __eq__(self, other) -> bool:
        return isinstance(other, $className) and all(
            getattr(self, attr) == getattr(other, attr)
            for attr in self.__slots__
        )
            |]
compileTypeExpression :: TypeExpression -> CodeGen Code
compileTypeExpression (TypeIdentifier i) = return $ toClassName i
compileTypeExpression (MapModifier k v) = do
    kExpr <- compileTypeExpression k
    vExpr <- compileTypeExpression v
    withStandardImport "typing" $ return [qq|typing.Mapping[$kExpr, $vExpr]|]
compileTypeExpression modifier = do
    expr <- compileTypeExpression typeExpr
    withStandardImport "typing" $ return [qq|typing.$className[$expr]|]
  where
    typeExpr :: TypeExpression
    className :: T.Text
    (typeExpr, className) = case modifier of
        OptionModifier t' -> (t', "Optional")
        SetModifier t' -> (t', "AbstractSet")
        ListModifier t' -> (t', "Sequence")
        TypeIdentifier _ -> undefined  -- never happen!
        MapModifier _ _ -> undefined  -- never happen!

compileTypeDeclaration :: TypeDeclaration -> CodeGen Code
compileTypeDeclaration (TypeDeclaration typename (Alias ctype) _) = do
    ctypeExpr <- compileTypeExpression ctype
    return [qq|
        # TODO: docstring
        {toClassName' typename} = $ctypeExpr
    |]
compileTypeDeclaration (TypeDeclaration typename (BoxedType itype) _) = do
    let className = toClassName' typename
    itypeExpr <- compileTypeExpression itype
    withStandardImport "typing" $
        withThirdPartyImports [ ( "nirum.validate"
                                , ["validate_boxed_type"]
                                )
                              , ("nirum.serialize", ["serialize_boxed_type"])
                              , ( "nirum.deserialize"
                                , ["deserialize_boxed_type"]
                                )
                              ] $
            return [qq|
class $className:
    # TODO: docstring

    def __init__(self, value: $itypeExpr) -> None:
        validate_boxed_type(value, $itypeExpr)
        self.value = value  # type: $itypeExpr

    def __eq__(self, other) -> bool:
        return (isinstance(other, $className) and
                self.value == other.value)

    def __hash__(self) -> int:
        return hash(self.value)

    def __nirum_serialize__(self) -> typing.Mapping[str, typing.Any]:
        return serialize_boxed_type(self)

    @classmethod
    def __nirum_deserialize__(
        cls: type, value: typing.Mapping[str, typing.Any]
    ) -> '{className}':
        return deserialize_boxed_type(cls, value)

    def __repr__(self) -> str:
        return '\{0.__module__\}.\{0.__qualname__\}(\{1!r\})'.format(
            type(self), self.value
        )
            |]
compileTypeDeclaration (TypeDeclaration typename (EnumType members) _) = do
    let className = toClassName' typename
        memberNames = T.intercalate
            "\n    "
            [ [qq|{toAttributeName' memberName} = '{toSnakeCaseText bn}'|]
            | EnumMember memberName@(Name _ bn) _ <- toList members
            ]
    withStandardImport "enum" $ return [qq|
class $className(enum.Enum):
    # TODO: docstring

    $memberNames

    def __nirum_serialize__(self) -> str:
        return self.value

    @classmethod
    def __nirum_deserialize__(cls: type, value: str) -> '{className}':
        return cls(value.replace('-', '_'))  # FIXME: validate input
    |]
compileTypeDeclaration (TypeDeclaration typename (RecordType fields) _) = do
    typeExprCodes <- mapM compileTypeExpression
        [typeExpr | (Field _ typeExpr _) <- toList fields]
    let className = toClassName' typename
        fieldNames = map toAttributeName' [ name
                                          | (Field name _ _) <- toList fields
                                          ]
        nameNTypes = zip fieldNames typeExprCodes
        slotTypes = toIndentedCodes
            (\(n, t) -> [qq|'{n}': {t}|]) nameNTypes ",\n        "
        slots = toIndentedCodes (\n -> [qq|'{n}'|]) fieldNames ",\n        "
        initialArgs = toIndentedCodes
            (\(n, t) -> [qq|{n}: {t}|]) nameNTypes ", "
        initialValues = toIndentedCodes
            (\n -> [qq|self.{n} = {n}|]) fieldNames "\n        "
        nameMaps = toIndentedCodes
            (\(Name f b) ->
                [qq|('{toAttributeName f}', '{toSnakeCaseText b}')|])
            [name | Field name _ _ <- toList fields, N.isComplex name]
            ",\n        "
    withStandardImport "typing" $
        withThirdPartyImports [ ( "nirum.validate"
                                , ["validate_record_type"]
                                )
                              , ("nirum.serialize", ["serialize_record_type"])
                              , ( "nirum.deserialize"
                                , ["deserialize_record_type"])
                              , ("nirum.constructs", ["NameDict"])
                              ] $
            return [qq|
class $className:
    # TODO: docstring

    __slots__ = (
        $slots,
    )
    __nirum_record_behind_name__ = '{toSnakeCaseText $ N.behindName typename}'
    __nirum_field_types__ = \{
        $slotTypes
    \}
    __nirum_field_names__ = NameDict([
        $nameMaps
    ])

    def __init__(self, $initialArgs) -> None:
        $initialValues
        validate_record_type(self)

    def __repr__(self) -> str:
        return '\{0.__module__\}.\{0.__qualname__\}(\{1\})'.format(
            type(self),
            ', '.join('\{\}=\{\}'.format(attr, getattr(self, attr))
                      for attr in self.__slots__)
        )

    def __eq__(self, other) -> bool:
        return isinstance(other, $className) and all(
            getattr(self, attr) == getattr(other, attr)
            for attr in self.__slots__
        )

    def __nirum_serialize__(self) -> typing.Mapping[str, typing.Any]:
        return serialize_record_type(self)

    @classmethod
    def __nirum_deserialize__(cls: type, value) -> '{className}':
        return deserialize_record_type(cls, value)
                        |]
compileTypeDeclaration (TypeDeclaration typename (UnionType tags) _) = do
    fieldCodes <- mapM (uncurry (compileUnionTag typename)) tagNameNFields
    let className = toClassName' typename
        fieldCodes' = T.intercalate "\n\n" fieldCodes
        enumMembers = toIndentedCodes
            (\(t, b) -> [qq|$t = '{b}'|]) enumMembers' "\n        "
    withStandardImport "typing" $
        withStandardImport "enum" $
            withThirdPartyImports [ ( "nirum.serialize"
                                    , ["serialize_union_type"])
                                  , ( "nirum.deserialize"
                                    , ["deserialize_union_type"])
                                  , ("nirum.constructs", ["NameDict"])
                                  ] $
                return [qq|
class $className:

    __nirum_union_behind_name__ = '{toSnakeCaseText $ N.behindName typename}'
    __nirum_field_names__ = NameDict([
        $nameMaps
    ])

    class Tag(enum.Enum):
        $enumMembers

    def __init__(self, *args, **kwargs):
        raise NotImplementedError(
            "\{0.__module__\}.\{0.__qualname__\} cannot be instantiated "
            "since it is an abstract class.  Instantiate a concrete subtype "
            "of it instead.".format(
                type(self)
            )
        )

    def __nirum_serialize__(self) -> typing.Mapping[str, typing.Any]:
        return serialize_union_type(self)

    @classmethod
    def __nirum_deserialize__(cls: type, value) -> '{className}':
        return deserialize_union_type(cls, value)


$fieldCodes'
            |]
  where
    tagNameNFields :: [(Name, DeclarationSet Field)]
    tagNameNFields = [ (tagName, fields)
                     | (Tag tagName fields _) <- toList tags
                     ]
    enumMembers' :: [(T.Text, T.Text)]
    enumMembers' = [ ( toAttributeName' tagName
                     , toSnakeCaseText $ N.behindName tagName
                     )
                   | (Tag tagName _ _) <- toList tags
                   ]
    nameMaps :: T.Text
    nameMaps = toIndentedCodes
        (\(Name f b) -> [qq|('{toAttributeName f}', '{toSnakeCaseText b}')|])
        [name | (name, _) <- tagNameNFields, N.isComplex name]
        ",\n        "

compileModuleBody :: Module -> CodeGen Code
compileModuleBody Module { types = types' } = do
    typeCodes <- mapM compileTypeDeclaration (toList types')
    let moduleCode = T.intercalate "\n\n" typeCodes
    return [qq|
# TODO: docs
$moduleCode
    |]

compileModule :: Module -> Code
compileModule module' =
    [qq|
{imports $ standardImports code'}

{fromImports $ localImports code'}

{fromImports $ thirdPartyImports code'}

Float64 = float  # FIXME
Bigint = int
Text = str

{code code'}
    |]
  where
    code' :: CodeGen T.Text
    code' = compileModuleBody module'
    imports :: S.Set T.Text -> T.Text
    imports importSet =
        if S.null importSet
        then ""
        else "import " `T.append` T.intercalate "," (S.elems importSet)
    fromImports :: M.Map T.Text (S.Set T.Text) -> T.Text
    fromImports importMap =
        T.intercalate "\n"
            [ [qq|from $from import {T.intercalate ", " $ S.elems vars}|]
            | (from, vars) <- M.assocs importMap
            ]
