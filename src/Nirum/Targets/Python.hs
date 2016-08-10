{-# LANGUAGE ExtendedDefaultRules, OverloadedLists, OverloadedStrings,
             QuasiQuotes #-}
module Nirum.Targets.Python ( Code
                            , CodeGen( code
                                     , localImports
                                     , standardImports
                                     , thirdPartyImports
                                     )
                            , Source( Source
                                    , sourceModule
                                    , sourcePackage
                                    )
                            , compileError
                            , compileModule
                            , compilePackage
                            , compilePrimitiveType
                            , compileTypeDeclaration
                            , compileTypeExpression
                            , hasError
                            , toAttributeName
                            , toClassName
                            , toImportPath
                            , toNamePair
                            , withLocalImport
                            , withStandardImport
                            , withThirdPartyImports
                            ) where

import qualified Data.List as L
import GHC.Exts (IsList(toList))

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.FilePath (joinPath)
import Text.InterpolatedString.Perl6 (qq)

import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Identifier ( Identifier
                                   , toPascalCaseText
                                   , toSnakeCaseText
                                   , toString
                                   )
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Constructs.Name (Name(Name))
import qualified Nirum.Constructs.Name as N
import Nirum.Constructs.Service ( Method(Method, methodName)
                                , Parameter(Parameter)
                                , Service(Service)
                                )
import Nirum.Constructs.TypeDeclaration ( EnumMember(EnumMember)
                                        , Field(Field)
                                        , PrimitiveTypeIdentifier(..)
                                        , Tag(Tag)
                                        , Type( Alias
                                              , BoxedType
                                              , EnumType
                                              , PrimitiveType
                                              , RecordType
                                              , UnionType
                                              )
                                        , TypeDeclaration( Import
                                                         , ServiceDeclaration
                                                         , TypeDeclaration
                                                         )
                                        )
import Nirum.Constructs.TypeExpression ( TypeExpression( ListModifier
                                                       , MapModifier
                                                       , OptionModifier
                                                       , SetModifier
                                                       , TypeIdentifier
                                                       )
                                       )
import Nirum.Package ( BoundModule
                     , Package(modules)
                     , TypeLookup(Imported, Local, Missing)
                     , lookupType
                     , resolveBoundModule
                     , types
                     )

data Source = Source { sourcePackage :: Package
                     , sourceModule :: BoundModule
                     } deriving (Eq, Ord, Show)

type Code = T.Text
type CompileError = T.Text

data CodeGen a = CodeGen { standardImports :: S.Set T.Text
                         , thirdPartyImports :: M.Map T.Text (S.Set T.Text)
                         , localImports :: M.Map T.Text (S.Set T.Text)
                         , code :: a
                         }
               | CodeGenError CompileError
               deriving (Eq, Ord, Show)

instance Functor CodeGen where
    fmap f codeGen = pure f <*> codeGen

instance Applicative CodeGen where
    pure = return
    c@CodeGen { code = f } <*> codeGen = codeGen >>= \x -> c { code = f x }
    (CodeGenError m) <*> _ = CodeGenError m

instance Monad CodeGen where
    return code' = CodeGen { standardImports = []
                           , thirdPartyImports = []
                           , localImports = []
                           , code = code'
                           }
    (CodeGen si ti li c) >>= f = case f c of
        (CodeGen si' ti' li' code') ->
            let stdImports = S.union si si'
                thirdPartyImports' = M.unionWith S.union ti ti'
                localImports' = M.unionWith S.union li li'
            in
                CodeGen stdImports thirdPartyImports' localImports' code'
        (CodeGenError m) -> CodeGenError m
    (CodeGenError m) >>= _ = CodeGenError m

    fail = CodeGenError . T.pack

hasError :: CodeGen a -> Bool
hasError (CodeGenError _) = True
hasError _ = False

compileError :: CodeGen a -> Maybe CompileError
compileError CodeGen {} = Nothing
compileError (CodeGenError m) = Just m

withStandardImport :: T.Text -> CodeGen a -> CodeGen a
withStandardImport module' c@CodeGen { standardImports = si } =
    c { standardImports = S.insert module' si }
withStandardImport _ c@(CodeGenError _) = c

withThirdPartyImports :: [(T.Text, S.Set T.Text)] -> CodeGen a -> CodeGen a
withThirdPartyImports imports c@CodeGen { thirdPartyImports = ti } =
    c { thirdPartyImports = L.foldl (M.unionWith S.union) ti importList }
  where
    importList :: [M.Map T.Text (S.Set T.Text)]
    importList = map (uncurry M.singleton) imports
withThirdPartyImports _ c@(CodeGenError _) = c

withLocalImport :: T.Text -> T.Text -> CodeGen a -> CodeGen a
withLocalImport module' object c@CodeGen { localImports = li } =
    c { localImports = M.insertWith S.union module' [object] li }
withLocalImport _ _ c@(CodeGenError _) = c

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

toImportPath :: ModulePath -> T.Text
toImportPath = T.intercalate "." . map toAttributeName . toList

toNamePair :: Name -> T.Text
toNamePair (Name f b) = [qq|('{toAttributeName f}', '{toSnakeCaseText b}')|]

toIndentedCodes :: (a -> T.Text) -> [a] -> T.Text -> T.Text
toIndentedCodes f traversable concatenator =
    T.intercalate concatenator $ map f traversable


compileUnionTag :: Source
                -> Name
                -> Name
                -> DS.DeclarationSet Field
                -> CodeGen Code
compileUnionTag source parentname typename fields = do
    typeExprCodes <- mapM (compileTypeExpression source)
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
            toNamePair
            [name | Field name _ _ <- toList fields]
            ",\n        "
        parentClass = toClassName' parentname
    withStandardImport "typing" $
        withThirdPartyImports [ ("nirum.validate", ["validate_union_type"])
                              , ("nirum.constructs", ["name_dict_type"])
                              ] $
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
    __nirum_tag_names__ = name_dict_type([
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

compilePrimitiveType :: PrimitiveTypeIdentifier -> CodeGen Code
compilePrimitiveType primitiveTypeIdentifier =
    case primitiveTypeIdentifier of
        Bool -> return "bool"
        Bigint -> return "int"
        Decimal -> withStandardImport "decimal" $ return "decimal.Decimal"
        Int32 -> return "int"
        Int64 -> return "int"
        Float32 -> return "float"
        Float64 -> return "float"
        Text -> return "str"
        Binary -> return "bytes"
        Date -> withStandardImport "datetime" $ return "datetime.date"
        Datetime -> withStandardImport "datetime" $ return "datetime.datetime"
        Uuid -> withStandardImport "uuid" $ return"uuid.UUID"
        Uri -> return "str"

compileTypeExpression :: Source -> TypeExpression -> CodeGen Code
compileTypeExpression Source { sourceModule = boundModule } (TypeIdentifier i) =
    case lookupType i boundModule of
        Missing -> fail $ "undefined identifier: " ++ toString i
        Imported _ (PrimitiveType p _) -> compilePrimitiveType p
        Imported m _ ->
            withThirdPartyImports [(toImportPath m, [toClassName i])] $
                return $ toClassName i
        Local _ -> return $ toClassName i
compileTypeExpression source (MapModifier k v) = do
    kExpr <- compileTypeExpression source k
    vExpr <- compileTypeExpression source v
    withStandardImport "typing" $ return [qq|typing.Mapping[$kExpr, $vExpr]|]
compileTypeExpression source modifier = do
    expr <- compileTypeExpression source typeExpr
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

compileTypeDeclaration :: Source -> TypeDeclaration -> CodeGen Code
compileTypeDeclaration _ (TypeDeclaration _ (PrimitiveType _ _) _ _) =
    return ""  -- never used
compileTypeDeclaration src (TypeDeclaration typename (Alias ctype) _ _) = do
    ctypeExpr <- compileTypeExpression src ctype
    return [qq|
# TODO: docstring
{toClassName' typename} = $ctypeExpr
    |]
compileTypeDeclaration src (TypeDeclaration typename (BoxedType itype) _ _) = do
    let className = toClassName' typename
    itypeExpr <- compileTypeExpression src itype
    withStandardImport "typing" $
        withThirdPartyImports [ ("nirum.validate", ["validate_boxed_type"])
                              , ("nirum.serialize", ["serialize_boxed_type"])
                              , ( "nirum.deserialize"
                                , ["deserialize_boxed_type"]
                                )
                              ] $
            return [qq|
class $className:
    # TODO: docstring

    __nirum_boxed_type__ = $itypeExpr

    def __init__(self, value: $itypeExpr) -> None:
        validate_boxed_type(value, $itypeExpr)
        self.value = value  # type: $itypeExpr

    def __eq__(self, other) -> bool:
        return (isinstance(other, $className) and
                self.value == other.value)

    def __hash__(self) -> int:
        return hash(self.value)

    def __nirum_serialize__(self) -> typing.Any:
        return serialize_boxed_type(self)

    @classmethod
    def __nirum_deserialize__(cls: type, value: typing.Any) -> '{className}':
        return deserialize_boxed_type(cls, value)

    def __repr__(self) -> str:
        return '\{0.__module__\}.\{0.__qualname__\}(\{1!r\})'.format(
            type(self), self.value
        )
            |]
compileTypeDeclaration _ (TypeDeclaration typename (EnumType members) _ _) = do
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
compileTypeDeclaration src (TypeDeclaration typename (RecordType fields) _ _) = do
    typeExprCodes <- mapM (compileTypeExpression src)
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
            toNamePair
            [name | Field name _ _ <- toList fields]
            ",\n        "
    withStandardImport "typing" $
        withThirdPartyImports [ ( "nirum.validate"
                                , ["validate_record_type"]
                                )
                              , ("nirum.serialize", ["serialize_record_type"])
                              , ( "nirum.deserialize"
                                , ["deserialize_record_type"])
                              , ("nirum.constructs", ["name_dict_type"])
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
    __nirum_field_names__ = name_dict_type([
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
compileTypeDeclaration src (TypeDeclaration typename (UnionType tags) _ _) = do
    fieldCodes <- mapM (uncurry (compileUnionTag src typename)) tagNameNFields
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
                                  , ("nirum.constructs", ["name_dict_type"])
                                  ] $
                return [qq|
class $className:

    __nirum_union_behind_name__ = '{toSnakeCaseText $ N.behindName typename}'
    __nirum_field_names__ = name_dict_type([
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
    tagNameNFields :: [(Name, DS.DeclarationSet Field)]
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
        toNamePair
        [name | (name, _) <- tagNameNFields]
        ",\n        "
compileTypeDeclaration src (ServiceDeclaration name (Service methods) _ _) = do
    let methods' = toList methods
    methodMetadata <- mapM compileMethodMetadata methods'
    let methodMetadata' = commaNl methodMetadata
    dummyMethods <- mapM compileMethod methods'
    let dummyMethods' = T.intercalate "\n\n" dummyMethods
    withThirdPartyImports [ ("nirum.constructs", ["name_dict_type"])
                          , ("nirum.rpc", ["service_type"])
                          ] $
        return [qq|
class $className(service_type):

    __nirum_service_methods__ = \{
        {methodMetadata'}
    \}
    __nirum_method_names__ = name_dict_type([
        $methodNameMap
    ])

    {dummyMethods'}
|]
  where
    className :: T.Text
    className = toClassName' name
    commaNl :: [T.Text] -> T.Text
    commaNl = T.intercalate ",\n"
    compileMethod :: Method -> CodeGen Code
    compileMethod (Method mName params rtype _ _) = do
        let mName' = toAttributeName' mName
        params' <- mapM compileParameter $ toList params
        rtypeExpr <- compileTypeExpression src rtype
        return [qq|
    def {mName'}(self, {commaNl params'}) -> $rtypeExpr:
        raise NotImplementedError('$className has to implement {mName'}()')
|]
    compileParameter :: Parameter -> CodeGen Code
    compileParameter (Parameter pName pType _) = do
        pTypeExpr <- compileTypeExpression src pType
        return [qq|{toAttributeName' pName}: $pTypeExpr|]
    compileMethodMetadata :: Method -> CodeGen Code
    compileMethodMetadata (Method mName params rtype _ _) = do
        let params' = toList params :: [Parameter]
        rtypeExpr <- compileTypeExpression src rtype
        paramMetadata <- mapM compileParameterMetadata params'
        let paramMetadata' = commaNl paramMetadata
        withThirdPartyImports [("nirum.constructs", ["name_dict_type"])] $
            return [qq|'{toAttributeName' mName}': \{
                '_return': $rtypeExpr,
                '_names': name_dict_type([{paramNameMap params'}]),
                {paramMetadata'}
            \}|]
    compileParameterMetadata :: Parameter -> CodeGen Code
    compileParameterMetadata (Parameter pName pType _) = do
        let pName' = toAttributeName' pName
        pTypeExpr <- compileTypeExpression src pType
        return [qq|'{pName'}': $pTypeExpr|]
    methodNameMap :: T.Text
    methodNameMap = toIndentedCodes
        toNamePair
        [mName | Method { methodName = mName } <- toList methods]
        ",\n        "
    paramNameMap :: [Parameter] -> T.Text
    paramNameMap params = toIndentedCodes
        toNamePair [pName | Parameter pName _ _ <- params] ",\n        "
compileTypeDeclaration _ (Import _ _) =
    return ""  -- Nothing to compile

compileModuleBody :: Source -> CodeGen Code
compileModuleBody src@Source { sourceModule = boundModule } = do
    let types' = types boundModule
    typeCodes <- mapM (compileTypeDeclaration src) $ toList types'
    let moduleCode = T.intercalate "\n\n" typeCodes
    return [qq|
# TODO: docs
$moduleCode
    |]

compileModule :: Source -> Either CompileError Code
compileModule source =
    case code' of
        CodeGenError errMsg -> Left errMsg
        CodeGen {} -> Right [qq|
{imports $ standardImports code'}

{fromImports $ localImports code'}

{fromImports $ thirdPartyImports code'}

{code code'}
    |]
  where
    code' :: CodeGen T.Text
    code' = compileModuleBody source
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

compilePackage :: Package -> M.Map FilePath (Either CompileError Code)
compilePackage package =
    M.fromList [ ( toFilename modulePath'
                 , compileModule $ Source package boundModule
                 )
               | (modulePath', _) <- M.assocs (modules package)
               , Just boundModule <- [resolveBoundModule modulePath' package]
               ]
  where
    toFilename :: ModulePath -> FilePath
    toFilename mp =
        joinPath $ [ T.unpack (toAttributeName i)
                   | i <- toList mp
                   ] ++ ["__init__.py"]
