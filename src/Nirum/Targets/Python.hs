{-# LANGUAGE ExtendedDefaultRules, OverloadedLists, QuasiQuotes,
  TypeSynonymInstances, MultiParamTypeClasses #-}
module Nirum.Targets.Python ( Code
                            , CodeGen
                            , CodeGenContext ( localImports
                                             , standardImports
                                             , thirdPartyImports
                                             )
                            , CompileError
                            , InstallRequires ( InstallRequires
                                              , dependencies
                                              , optionalDependencies
                                              )
                            , Source ( Source
                                     , sourceModule
                                     , sourcePackage
                                     )
                            , PythonVersion ( Python2
                                            , Python3
                                            )
                            , addDependency
                            , addOptionalDependency
                            , compileModule
                            , compilePackage
                            , compilePrimitiveType
                            , compileTypeDeclaration
                            , compileTypeExpression
                            , emptyContext
                            , insertLocalImport
                            , insertStandardImport
                            , insertThirdPartyImports
                            , runCodeGen
                            , stringLiteral
                            , toAttributeName
                            , toClassName
                            , toImportPath
                            , toNamePair
                            , unionInstallRequires
                            ) where

import qualified Control.Monad.State as ST
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import GHC.Exts (IsList (toList))
import Text.Printf (printf)

import qualified Data.Map.Strict as M
import qualified Data.SemVer as SV
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.FilePath (joinPath)
import qualified Text.Email.Validate as E
import Text.InterpolatedString.Perl6 (qq)

import qualified Nirum.CodeGen as C
import Nirum.CodeGen (Failure)
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Identifier ( Identifier
                                   , toPascalCaseText
                                   , toSnakeCaseText
                                   , toString
                                   )
import Nirum.Constructs.ModulePath (ModulePath, ancestors)
import Nirum.Constructs.Name (Name (Name))
import qualified Nirum.Constructs.Name as N
import Nirum.Constructs.Service ( Method ( Method
                                         , methodName
                                         , parameters
                                         , returnType
                                         )
                                , Parameter (Parameter)
                                , Service (Service)
                                )
import Nirum.Constructs.TypeDeclaration ( EnumMember (EnumMember)
                                        , Field (Field, fieldName)
                                        , PrimitiveTypeIdentifier (..)
                                        , Tag (Tag)
                                        , Type ( Alias
                                               , EnumType
                                               , PrimitiveType
                                               , RecordType
                                               , UnboxedType
                                               , UnionType
                                               )
                                        , TypeDeclaration (..)
                                        )
import Nirum.Constructs.TypeExpression ( TypeExpression ( ListModifier
                                                        , MapModifier
                                                        , OptionModifier
                                                        , SetModifier
                                                        , TypeIdentifier
                                                        )
                                       )
import Nirum.Package ( BoundModule
                     , Package (Package, metadata, modules)
                     , TypeLookup (Imported, Local, Missing)
                     , lookupType
                     , resolveBoundModule
                     , types
                     )
import Nirum.Package.Metadata ( Author (Author, name, email)
                              , Metadata (authors, version)
                              )
import qualified Nirum.Package.ModuleSet as MS

data PythonVersion = Python2
                   | Python3
                   deriving (Eq, Ord, Show)
data Source = Source { sourcePackage :: Package
                     , sourceModule :: BoundModule
                     } deriving (Eq, Ord, Show)

type Code = T.Text
type CompileError = T.Text

instance Failure CodeGenContext CompileError where
    fromString = return . T.pack

data CodeGenContext
    = CodeGenContext { standardImports :: S.Set T.Text
                     , thirdPartyImports :: M.Map T.Text (S.Set T.Text)
                     , localImports :: M.Map T.Text (S.Set T.Text)
                     , pythonVersion :: PythonVersion
                     }
    deriving (Eq, Ord, Show)

python3SourceDirectory :: T.Text
python3SourceDirectory = "src"
python2SourceDirectory :: T.Text
python2SourceDirectory = "src-py2"

emptyContext :: PythonVersion -> CodeGenContext
emptyContext pythonVersion' = CodeGenContext { standardImports = []
                                             , thirdPartyImports = []
                                             , localImports = []
                                             , pythonVersion = pythonVersion'
                                             }

type CodeGen = C.CodeGen CodeGenContext CompileError

runCodeGen :: CodeGen a
           -> CodeGenContext
           -> (Either CompileError a, CodeGenContext)
runCodeGen = C.runCodeGen

insertStandardImport :: T.Text -> CodeGen ()
insertStandardImport module' = ST.modify insert'
  where
    insert' c@CodeGenContext { standardImports = si } =
        c { standardImports = S.insert module' si }

insertThirdPartyImports :: [(T.Text, S.Set T.Text)] -> CodeGen ()
insertThirdPartyImports imports = ST.modify insert'
  where
    insert' c@CodeGenContext { thirdPartyImports = ti } =
        c { thirdPartyImports = L.foldl (M.unionWith S.union) ti importList }
    importList :: [M.Map T.Text (S.Set T.Text)]
    importList = map (uncurry M.singleton) imports

insertLocalImport :: T.Text -> T.Text -> CodeGen ()
insertLocalImport module' object = ST.modify insert'
  where
    insert' c@CodeGenContext { localImports = li } =
        c { localImports = M.insertWith S.union module' [object] li }

insertTypingImport :: CodeGen ()
insertTypingImport = do
    pyVer <- getPythonVersion
    case pyVer of
        Python2 -> return ()
        Python3 -> insertStandardImport "typing"

insertEnumImport :: CodeGen ()
insertEnumImport = insertStandardImport "enum"

getPythonVersion :: CodeGen PythonVersion
getPythonVersion = fmap pythonVersion ST.get

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

stringLiteral :: T.Text -> T.Text
stringLiteral string =
    open $ T.concatMap esc string `T.snoc` '"'
  where
    open :: T.Text -> T.Text
    open = if T.any (> '\xff') string then T.append [qq|u"|] else T.cons '"'
    esc :: Char -> T.Text
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\t' = "\\t"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc c
        | c >= '\x10000' = T.pack $ printf "\\U%08x" c
        | c >= '\xff' = T.pack $ printf "\\u%04x" c
        | c < ' ' || c >= '\x7f' = T.pack $ printf "\\x%02x" c
        | otherwise = T.singleton c

toIndentedCodes :: (a -> T.Text) -> [a] -> T.Text -> T.Text
toIndentedCodes f traversable concatenator =
    T.intercalate concatenator $ map f traversable

quote :: T.Text -> T.Text
quote s = [qq|'{s}'|]


type ParameterName = Code
type ParameterType = Code
type ReturnType = Code

parameterCompiler :: CodeGen (ParameterName -> ParameterType -> Code)
parameterCompiler = do
    ver <- getPythonVersion
    return $ \ n t -> case ver of
                          Python2 -> n
                          Python3 -> [qq|$n: $t|]

returnCompiler :: CodeGen (ReturnType -> Code)
returnCompiler = do
    ver <- getPythonVersion
    return $ \ r -> case ver of
                        Python2 -> ""
                        Python3 -> [qq| -> $r|]

compileUnionTag :: Source
                -> Name
                -> Name
                -> DS.DeclarationSet Field
                -> CodeGen Code
compileUnionTag source parentname typename' fields = do
    typeExprCodes <- mapM (compileTypeExpression source)
        [typeExpr | (Field _ typeExpr _) <- toList fields]
    let className = toClassName' typename'
        tagNames = map (toAttributeName' . fieldName) (toList fields)
        nameNTypes = zip tagNames typeExprCodes
        slotTypes = toIndentedCodes
            (\ (n, t) -> [qq|'{n}': {t}|]) nameNTypes ",\n        "
        slots = if length tagNames == 1
                then [qq|'{head tagNames}'|] `T.snoc` ','
                else toIndentedCodes (\ n -> [qq|'{n}'|]) tagNames ",\n        "
        hashTuple = if null tagNames
            then "self.__nirum_tag__"
            else [qq|({attributes},)|] :: T.Text
          where
            attributes :: T.Text
            attributes = toIndentedCodes (\ n -> [qq|self.{n}|]) tagNames ", "
        initialArgs gen = toIndentedCodes (uncurry gen) nameNTypes ", "
        initialValues =
            toIndentedCodes (\ n -> [qq|self.{n} = {n}|]) tagNames "\n        "
        nameMaps = toIndentedCodes
            toNamePair
            (map fieldName $ toList fields)
            ",\n        "
        parentClass = toClassName' parentname
    insertTypingImport
    insertThirdPartyImports [ ("nirum.validate", ["validate_union_type"])
                            , ("nirum.constructs", ["name_dict_type"])
                            ]
    arg <- parameterCompiler
    ret <- returnCompiler
    return [qq|
class $className($parentClass):
    # TODO: docstring

    __slots__ = (
        $slots
    )
    __nirum_tag__ = $parentClass.Tag.{toAttributeName' typename'}
    __nirum_tag_types__ = \{
        $slotTypes
    \}
    __nirum_tag_names__ = name_dict_type([
        $nameMaps
    ])

    def __init__(self, {initialArgs arg}){ ret "None" }:
        $initialValues
        validate_union_type(self)

    def __repr__(self){ ret "str" }:
        return '\{0\}(\{1\})'.format(
            typing._type_repr(self),
            ', '.join('\{\}=\{\}'.format(attr, getattr(self, attr))
                      for attr in self.__slots__)
        )

    def __eq__(self, other){ ret "bool" }:
        return isinstance(other, $className) and all(
            getattr(self, attr) == getattr(other, attr)
            for attr in self.__slots__
        )

    def __ne__(self, other){ ret "bool" }:
        return not self == other

    def __hash__(self){ ret "int" }:
        return hash($hashTuple)
|]
compilePrimitiveType :: PrimitiveTypeIdentifier -> CodeGen Code
compilePrimitiveType primitiveTypeIdentifier = do
    pyVer <- getPythonVersion
    case (primitiveTypeIdentifier, pyVer) of
        (Bool, _) -> return "bool"
        (Bigint, _) -> return "int"
        (Decimal, _) -> do
            insertStandardImport "decimal"
            return "decimal.Decimal"
        (Int32, _) -> return "int"
        (Int64, Python2) -> return "long"
        (Int64, Python3) -> return "int"
        (Float32, _) -> return "float"
        (Float64, _) -> return "float"
        (Text, Python2) -> return "unicode"
        (Text, Python3) -> return "str"
        (Binary, _) -> return "bytes"
        (Date, _) -> do
            insertStandardImport "datetime"
            return "datetime.date"
        (Datetime, _) -> do
            insertStandardImport "datetime"
            return "datetime.datetime"
        (Uuid, _) -> insertStandardImport "uuid" >> return "uuid.UUID"
        (Uri, Python2) -> return "unicode"
        (Uri, Python3) -> return "str"

compileTypeExpression :: Source -> TypeExpression -> CodeGen Code
compileTypeExpression Source { sourceModule = boundModule } (TypeIdentifier i) =
    case lookupType i boundModule of
        Missing -> fail $ "undefined identifier: " ++ toString i
        Imported _ (PrimitiveType p _) -> compilePrimitiveType p
        Imported m _ -> do
            insertThirdPartyImports [(toImportPath m, [toClassName i])]
            return $ toClassName i
        Local _ -> return $ toClassName i
compileTypeExpression source (MapModifier k v) = do
    kExpr <- compileTypeExpression source k
    vExpr <- compileTypeExpression source v
    insertTypingImport
    return [qq|typing.Mapping[$kExpr, $vExpr]|]
compileTypeExpression source modifier = do
    expr <- compileTypeExpression source typeExpr
    insertTypingImport
    return [qq|typing.$className[$expr]|]
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
compileTypeDeclaration _ TypeDeclaration { type' = PrimitiveType {} } =
    return ""  -- never used
compileTypeDeclaration src TypeDeclaration { typename = typename'
                                           , type' = Alias ctype } = do
    ctypeExpr <- compileTypeExpression src ctype
    return [qq|
# TODO: docstring
{toClassName' typename'} = $ctypeExpr
    |]
compileTypeDeclaration src TypeDeclaration { typename = typename'
                                           , type' = UnboxedType itype } = do
    let className = toClassName' typename'
    itypeExpr <- compileTypeExpression src itype
    insertTypingImport
    insertThirdPartyImports [ ("nirum.validate", ["validate_boxed_type"])
                            , ("nirum.serialize", ["serialize_boxed_type"])
                            , ("nirum.deserialize", ["deserialize_boxed_type"])
                            ]
    arg <- parameterCompiler
    ret <- returnCompiler
    return [qq|
class $className(object):
    # TODO: docstring

    __nirum_inner_type__ = $itypeExpr

    def __init__(self, { arg "value" itypeExpr }){ ret "None" }:
        validate_boxed_type(value, $itypeExpr)
        self.value = value  # type: $itypeExpr

    def __eq__(self, other){ ret "bool" }:
        return (isinstance(other, $className) and
                self.value == other.value)

    def __ne__(self, other){ ret "bool" }:
        return not self == other

    def __hash__(self){ ret "int" }:
        return hash(self.value)

    def __nirum_serialize__(self){ ret "typing.Any" }:
        return serialize_boxed_type(self)

    @classmethod
    def __nirum_deserialize__(
        {arg "cls" "type"},
        {arg "value" "typing.Any"}
    ){ ret $ quote className }:
        return deserialize_boxed_type(cls, value)

    def __repr__(self){ ret "str" }:
        return '\{0\}(\{1!r\})'.format(
            typing._type_repr(self), self.value
        )

    def __hash__(self) -> int:
        return hash(self.value)
|]
compileTypeDeclaration _ TypeDeclaration { typename = typename'
                                         , type' = EnumType members } = do
    let className = toClassName' typename'
        memberNames = T.intercalate
            "\n    "
            [ [qq|{toAttributeName' memberName} = '{toSnakeCaseText bn}'|]
            | EnumMember memberName@(Name _ bn) _ <- toList members
            ]
    insertEnumImport
    arg <- parameterCompiler
    ret <- returnCompiler
    return [qq|
class $className(enum.Enum):
    # TODO: docstring

    $memberNames

    def __nirum_serialize__(self){ ret "str" }:
        return self.value

    @classmethod
    def __nirum_deserialize__(
        {arg "cls" "type"},
        {arg "value" "str"}
    ){ ret $ quote className }:
        return cls(value.replace('-', '_'))  # FIXME: validate input
|]
compileTypeDeclaration src TypeDeclaration { typename = typename'
                                           , type' = RecordType fields } = do
    typeExprCodes <- mapM (compileTypeExpression src)
        [typeExpr | (Field _ typeExpr _) <- toList fields]
    let className = toClassName' typename'
        fieldNames = map toAttributeName' [ name'
                                          | (Field name' _ _) <- toList fields
                                          ]
        nameNTypes = zip fieldNames typeExprCodes
        slotTypes = toIndentedCodes
            (\ (n, t) -> [qq|'{n}': {t}|]) nameNTypes ",\n        "
        slots = toIndentedCodes (\ n -> [qq|'{n}'|]) fieldNames ",\n        "
        initialArgs gen = toIndentedCodes (uncurry gen) nameNTypes ", "
        initialValues = toIndentedCodes
            (\ n -> [qq|self.{n} = {n}|]) fieldNames "\n        "
        nameMaps = toIndentedCodes
            toNamePair
            (map fieldName $ toList fields)
            ",\n        "
        hashText = toIndentedCodes (\ n -> [qq|self.{n}|]) fieldNames ", "
    insertTypingImport
    insertThirdPartyImports [ ("nirum.validate", ["validate_record_type"])
                            , ("nirum.serialize", ["serialize_record_type"])
                            , ("nirum.deserialize", ["deserialize_record_type"])
                            , ("nirum.constructs", ["name_dict_type"])
                            ]
    arg <- parameterCompiler
    ret <- returnCompiler
    let clsType = arg "cls" "type"
    return [qq|
class $className(object):
    # TODO: docstring

    __slots__ = (
        $slots,
    )
    __nirum_record_behind_name__ = '{toSnakeCaseText $ N.behindName typename'}'
    __nirum_field_types__ = \{
        $slotTypes
    \}
    __nirum_field_names__ = name_dict_type([
        $nameMaps
    ])

    def __init__(self, {initialArgs arg}){ret "None"}:
        $initialValues
        validate_record_type(self)

    def __repr__(self){ret "bool"}:
        return '\{0\}(\{1\})'.format(
            typing._type_repr(self),
            ', '.join('\{\}=\{\}'.format(attr, getattr(self, attr))
                      for attr in self.__slots__)
        )

    def __eq__(self, other){ret "bool"}:
        return isinstance(other, $className) and all(
            getattr(self, attr) == getattr(other, attr)
            for attr in self.__slots__
        )

    def __ne__(self, other){ ret "bool" }:
        return not self == other

    def __nirum_serialize__(self){ret "typing.Mapping[str, typing.Any]"}:
        return serialize_record_type(self)

    @classmethod
    def __nirum_deserialize__($clsType, value){ ret $ quote className }:
        return deserialize_record_type(cls, value)

    def __hash__(self){ret "int"}:
        return hash(($hashText,))
|]
compileTypeDeclaration src TypeDeclaration { typename = typename'
                                           , type' = UnionType tags } = do
    fieldCodes <- mapM (uncurry (compileUnionTag src typename')) tagNameNFields
    let className = toClassName' typename'
        fieldCodes' = T.intercalate "\n\n" fieldCodes
        enumMembers = toIndentedCodes
            (\ (t, b) -> [qq|$t = '{b}'|]) enumMembers' "\n        "
    insertTypingImport
    insertEnumImport
    insertThirdPartyImports [ ("nirum.serialize", ["serialize_union_type"])
                            , ("nirum.deserialize", ["deserialize_union_type"])
                            , ("nirum.constructs", ["name_dict_type"])
                            ]
    arg <- parameterCompiler
    ret <- returnCompiler
    return [qq|
class $className(object):

    __nirum_union_behind_name__ = '{toSnakeCaseText $ N.behindName typename'}'
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

    def __nirum_serialize__(self){ ret "typing.Mapping[str, typing.Any]" }:
        return serialize_union_type(self)

    @classmethod
    def __nirum_deserialize__(
        {arg "cls" "type"}, value
    ){ ret $ quote className }:
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
        [name' | (name', _) <- tagNameNFields]
        ",\n        "
compileTypeDeclaration src ServiceDeclaration { serviceName = name'
                                              , service = Service methods } = do
    let methods' = toList methods
    methodMetadata <- mapM compileMethodMetadata methods'
    let methodMetadata' = commaNl methodMetadata
    dummyMethods <- mapM compileMethod methods'
    clientMethods <- mapM compileClientMethod methods'
    let dummyMethods' = T.intercalate "\n\n" dummyMethods
        clientMethods' = T.intercalate "\n\n" clientMethods
    insertStandardImport "json"
    insertThirdPartyImports [ ("nirum.constructs", ["name_dict_type"])
                            , ("nirum.deserialize", ["deserialize_meta"])
                            , ("nirum.serialize", ["serialize_meta"])
                            , ("nirum.rpc", [ "service_type"
                                            , "client_type"
                                            ])
                            ]
    return [qq|
class $className(service_type):

    __nirum_service_methods__ = \{
        {methodMetadata'}
    \}
    __nirum_method_names__ = name_dict_type([
        $methodNameMap
    ])

    {dummyMethods'}


# FIXME client MUST be generated & saved on diffrent module
#       where service isn't included.
class {className}_Client(client_type, $className):
    {clientMethods'}
    pass
|]
  where
    className :: T.Text
    className = toClassName' name'
    commaNl :: [T.Text] -> T.Text
    commaNl = T.intercalate ",\n"
    compileMethod :: Method -> CodeGen Code
    compileMethod (Method mName params rtype _etype _anno) = do
        let mName' = toAttributeName' mName
        params' <- mapM compileMethodParameter $ toList params
        rtypeExpr <- compileTypeExpression src rtype
        ret <- returnCompiler
        return [qq|
    def {mName'}(self, {commaNl params'}){ ret rtypeExpr }:
        raise NotImplementedError('$className has to implement {mName'}()')
|]
    compileMethodParameter :: Parameter -> CodeGen Code
    compileMethodParameter (Parameter pName pType _) = do
        pTypeExpr <- compileTypeExpression src pType
        arg <- parameterCompiler
        return [qq|{arg (toAttributeName' pName) pTypeExpr}|]
    compileMethodMetadata :: Method -> CodeGen Code
    compileMethodMetadata Method { methodName = mName
                                 , parameters = params
                                 , returnType = rtype
                                 } = do
        let params' = toList params :: [Parameter]
        rtypeExpr <- compileTypeExpression src rtype
        paramMetadata <- mapM compileParameterMetadata params'
        let paramMetadata' = commaNl paramMetadata
        insertThirdPartyImports [("nirum.constructs", ["name_dict_type"])]
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
    compileClientPayload :: Parameter -> CodeGen Code
    compileClientPayload (Parameter pName _ _) = do
        let pName' = toAttributeName' pName
        return [qq|meta['_names']['{pName'}']: serialize_meta({pName'})|]
    compileClientMethod :: Method -> CodeGen Code
    compileClientMethod Method { methodName = mName
                               , parameters = params
                               , returnType = rtype
                               } = do
        let clientMethodName' = toAttributeName' mName
        params' <- mapM compileMethodParameter $ toList params
        rtypeExpr <- compileTypeExpression src rtype
        payloadArguments <- mapM compileClientPayload $ toList params
        ret <- returnCompiler
        return [qq|
    def {clientMethodName'}(self, {commaNl params'}){ ret rtypeExpr }:
        meta = self.__nirum_service_methods__['{clientMethodName'}']
        return deserialize_meta(
            meta['_return'],
            json.loads(
                self.remote_call(
                    self.__nirum_method_names__['{clientMethodName'}'],
                    payload=\{{commaNl payloadArguments}\}
                )
            )
        )
|]

compileTypeDeclaration _ Import {} =
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

data InstallRequires =
    InstallRequires { dependencies :: S.Set T.Text
                    , optionalDependencies :: M.Map (Int, Int) (S.Set T.Text)
                    } deriving (Eq, Ord, Show)

addDependency :: InstallRequires -> T.Text -> InstallRequires
addDependency requires package =
    requires { dependencies = S.insert package $ dependencies requires }

addOptionalDependency :: InstallRequires
                      -> (Int, Int)       -- ^ Python version already stasified
                      -> T.Text           -- ^ PyPI package name
                      -> InstallRequires
addOptionalDependency requires pyVer package =
    requires { optionalDependencies = newOptDeps }
  where
    oldOptDeps :: M.Map (Int, Int) (S.Set T.Text)
    oldOptDeps = optionalDependencies requires
    newOptDeps :: M.Map (Int, Int) (S.Set T.Text)
    newOptDeps = M.alter (Just . S.insert package . fromMaybe S.empty)
                         pyVer oldOptDeps

unionInstallRequires :: InstallRequires -> InstallRequires -> InstallRequires
unionInstallRequires a b =
    a { dependencies = S.union (dependencies a) (dependencies b)
      , optionalDependencies = M.unionWith S.union (optionalDependencies a)
                                                   (optionalDependencies b)
      }

compileModule :: PythonVersion
              -> Source
              -> Either CompileError (InstallRequires, Code)
compileModule pythonVersion' source =
    case runCodeGen code' $ emptyContext pythonVersion' of
        (Left errMsg, _) -> Left errMsg
        (Right code, context) -> codeWithDeps context $
            [qq|# -*- coding: utf-8 -*-
{imports $ standardImports context}

{fromImports $ localImports context}

{fromImports $ thirdPartyImports context}

{code}
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
    has :: S.Set T.Text -> T.Text -> Bool
    has set module' = module' `S.member` set ||
                      any (T.isPrefixOf $ module' `T.snoc` '.') set
    require :: T.Text -> T.Text -> S.Set T.Text -> S.Set T.Text
    require pkg module' set =
        if set `has` module' then S.singleton pkg else S.empty
    codeWithDeps :: CodeGenContext
                 -> Code
                 -> Either CompileError (InstallRequires, Code)
    codeWithDeps context c = Right (InstallRequires deps optDeps, c)
      where
        deps :: S.Set T.Text
        deps = require "nirum" "nirum" $ M.keysSet $ thirdPartyImports context
        optDeps :: M.Map (Int, Int) (S.Set T.Text)
        optDeps =
            [ ((3, 4), require "enum34" "enum" $ standardImports context)
            , ((3, 5), require "typing" "typing" $ standardImports context)
            ]

compilePackageMetadata :: Package -> InstallRequires -> Code
compilePackageMetadata package@Package { metadata = metadata' }
                       (InstallRequires deps optDeps) =
    [qq|# -*- coding: utf-8 -*-
import sys

from setuptools import setup, __version__ as setuptools_version

install_requires = [$pInstallRequires]
polyfill_requires = \{$pPolyfillRequires}

if polyfill_requires:
    # '<' operator for environment markers are supported since setuptools 17.1.
    # Read PEP 496 for details of environment markers.
    setup_requires = ['setuptools >= 17.1']
    if tuple(map(int, setuptools_version.split('.'))) < (17, 1):
        extras_require = \{}
        if 'bdist_wheel' not in sys.argv:
            for (major, minor), deps in polyfill_requires.items():
                if sys.version_info < (major, minor):
                    install_requires.extend(deps)
        envmarker = ":python_version=='\{0}.\{1}'"
        python_versions = [(2, 6), (2, 7),
                           (3, 3), (3, 4), (3, 5), (3, 6)]  # FIXME
        for pyver in python_versions:
            extras_require[envmarker.format(*pyver)] = list(\{
                d
                for v, vdeps in polyfill_requires.items()
                if pyver < v
                for d in vdeps
            })
    else:
        extras_require = \{
            ":python_version<'\{0}.\{1}'".format(*pyver): deps
            for pyver, deps in polyfill_requires.items()
        }
else:
    setup_requires = []
    extras_require = \{}


SOURCE_ROOT = '{python3SourceDirectory}'

if sys.version_info < (3, 0):
    SOURCE_ROOT = '{python2SourceDirectory}'

# TODO: description, long_description, url, license,
#       keywords, classifiers
setup(
    name='{pName}',
    version='{pVersion}',
    author=$author,
    author_email=$authorEmail,
    package_dir=\{'': SOURCE_ROOT},
    packages=[$pPackages],
    provides=[$pPackages],
    requires=[$pInstallRequires],
    setup_requires=setup_requires,
    install_requires=install_requires,
    extras_require=extras_require,
)
|]
  where
    csStrings :: [T.Text] -> T.Text
    csStrings [] = "None"
    csStrings s = stringLiteral $ T.intercalate ", " s
    pName :: Code
    pName = "TestPackage"  -- FIXME
    pVersion :: Code
    pVersion = SV.toText $ version metadata'
    strings :: [Code] -> Code
    strings values = T.intercalate ", " $ map stringLiteral (L.sort values)
    author :: Code
    author = csStrings [aName | Author { name = aName } <- authors metadata']
    authorEmail :: Code
    authorEmail = csStrings [ decodeUtf8 (E.toByteString e)
                            | Author { email = Just e } <- authors metadata'
                            ]
    pPackages :: Code
    pPackages = strings $ map toImportPath $ MS.keys $ modules package
    pInstallRequires :: Code
    pInstallRequires = strings $ S.toList deps
    pPolyfillRequires :: Code
    pPolyfillRequires = T.intercalate ", "
        [ [qq|($major, $minor): [{strings $ S.toList deps'}]|]
        | ((major, minor), deps') <- M.toList optDeps
        ]

compilePackage :: Package
               -> M.Map FilePath (Either CompileError Code)
compilePackage package =
    M.fromList $
        initFiles ++
        [ ( f
          , case cd of
                Left e -> Left e
                Right (_, cd') -> Right cd'
          )
        | (f, cd) <- modules'
        ] ++
        [("setup.py", Right $ compilePackageMetadata package installRequires)]
  where
    toPythonFilename :: ModulePath -> [FilePath]
    toPythonFilename mp = [ T.unpack (toAttributeName i)
                          | i <- toList mp
                          ] ++ ["__init__.py"]
    versionDirectoryList :: [(T.Text, PythonVersion)]
    versionDirectoryList = [ (python2SourceDirectory, Python2)
                           , (python3SourceDirectory, Python3)
                           ]
    toFilename :: T.Text -> ModulePath -> FilePath
    toFilename sourceRootDirectory mp =
        joinPath $ T.unpack sourceRootDirectory : toPythonFilename mp
    initFiles :: [(FilePath, Either CompileError Code)]
    initFiles = [ (toFilename sourceRootDirectory mp', Right "")
                | mp <- MS.keys (modules package)
                , mp' <- S.elems (ancestors mp)
                , (sourceRootDirectory, _) <- versionDirectoryList
                ]
    modules' :: [(FilePath, Either CompileError (InstallRequires, Code))]
    modules' =
        [ ( toFilename sourceRootDirectory modulePath'
          , compileModule pythonVersion' $ Source package boundModule
          )
        | (modulePath', _) <- MS.toAscList (modules package)
        , Just boundModule <- [resolveBoundModule modulePath' package]
        , (sourceRootDirectory, pythonVersion') <- versionDirectoryList
        ]
    installRequires :: InstallRequires
    installRequires = foldl unionInstallRequires
                            (InstallRequires [] [])
                            [deps | (_, Right (deps, _)) <- modules']
