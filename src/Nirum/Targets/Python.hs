{-# LANGUAGE ExtendedDefaultRules, OverloadedLists, QuasiQuotes #-}
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
                            , Source( Source
                                    , sourceModule
                                    , sourcePackage
                                    )
                            , addDependency
                            , addOptionalDependency
                            , compileModule
                            , compilePackage
                            , compilePrimitiveType
                            , compileTypeDeclaration
                            , compileTypeExpression
                            , emptyContext
                            , toAttributeName
                            , toClassName
                            , toImportPath
                            , toNamePair
                            , unionInstallRequires
                            , insertLocalImport
                            , insertStandardImport
                            , insertThirdPartyImports
                            , runCodeGen
                            ) where

import Control.Monad.State (modify)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import GHC.Exts (IsList(toList))

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.FilePath (joinPath)
import Text.InterpolatedString.Perl6 (qq)

import qualified Nirum.CodeGen as C
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Identifier ( Identifier
                                   , toPascalCaseText
                                   , toSnakeCaseText
                                   , toString
                                   )
import Nirum.Constructs.ModulePath (ModulePath, ancestors)
import Nirum.Constructs.Name (Name(Name))
import qualified Nirum.Constructs.Name as N
import Nirum.Constructs.Service ( Method( Method
                                        , methodName
                                        , parameters
                                        , returnType
                                        )
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
                                        , TypeDeclaration(..)
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

data CodeGenContext
    = CodeGenContext { standardImports :: S.Set T.Text
                     , thirdPartyImports :: M.Map T.Text (S.Set T.Text)
                     , localImports :: M.Map T.Text (S.Set T.Text)
                     }
    deriving (Eq, Ord, Show)

emptyContext :: CodeGenContext
emptyContext = CodeGenContext { standardImports = []
                              , thirdPartyImports = []
                              , localImports = []
                              }

type CodeGen = C.CodeGen CodeGenContext CompileError

runCodeGen :: CodeGen a -> CodeGenContext -> Either CompileError (a, CodeGenContext)
runCodeGen = C.runCodeGen

insertStandardImport :: T.Text -> CodeGen ()
insertStandardImport module' = modify insert'
  where
    insert' c@CodeGenContext { standardImports = si } =
        c { standardImports = S.insert module' si }

insertThirdPartyImports :: [(T.Text, S.Set T.Text)] -> CodeGen ()
insertThirdPartyImports imports = modify insert'
  where
    insert' c@CodeGenContext { thirdPartyImports = ti } =
        c { thirdPartyImports = L.foldl (M.unionWith S.union) ti importList }
    importList :: [M.Map T.Text (S.Set T.Text)]
    importList = map (uncurry M.singleton) imports

insertLocalImport :: T.Text -> T.Text -> CodeGen ()
insertLocalImport module' object = modify insert'
  where
    insert' c@CodeGenContext { localImports = li } =
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
compileUnionTag source parentname typename' fields = do
    typeExprCodes <- mapM (compileTypeExpression source)
        [typeExpr | (Field _ typeExpr _) <- toList fields]
    let className = toClassName' typename'
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
    insertStandardImport "typing"
    insertThirdPartyImports [ ("nirum.validate", ["validate_union_type"])
                            , ("nirum.constructs", ["name_dict_type"])
                            ]
    return [qq|
class $className($parentClass):
    # TODO: docstring

    __slots__ = (
        $slots,
    )
    __nirum_tag__ = $parentClass.Tag.{toAttributeName' typename'}
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
        Decimal -> insertStandardImport "decimal" >> return "decimal.Decimal"
        Int32 -> return "int"
        Int64 -> return "int"
        Float32 -> return "float"
        Float64 -> return "float"
        Text -> return "str"
        Binary -> return "bytes"
        Date -> insertStandardImport "datetime" >> return "datetime.date"
        Datetime -> insertStandardImport "datetime" >> return "datetime.datetime"
        Uuid -> insertStandardImport "uuid" >> return"uuid.UUID"
        Uri -> return "str"

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
    insertStandardImport "typing"
    return [qq|typing.Mapping[$kExpr, $vExpr]|]
compileTypeExpression source modifier = do
    expr <- compileTypeExpression source typeExpr
    insertStandardImport "typing"
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
compileTypeDeclaration _ TypeDeclaration { type' = PrimitiveType { } } =
    return ""  -- never used
compileTypeDeclaration src TypeDeclaration { typename = typename'
                                           , type' = Alias ctype } = do
    ctypeExpr <- compileTypeExpression src ctype
    return [qq|
# TODO: docstring
{toClassName' typename'} = $ctypeExpr
    |]
compileTypeDeclaration src TypeDeclaration { typename = typename'
                                           , type' = BoxedType itype } = do
    let className = toClassName' typename'
    itypeExpr <- compileTypeExpression src itype
    insertStandardImport "typing"
    insertThirdPartyImports [ ("nirum.validate", ["validate_boxed_type"])
                            , ("nirum.serialize", ["serialize_boxed_type"])
                            , ("nirum.deserialize", ["deserialize_boxed_type"])
                            ]
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
compileTypeDeclaration _ TypeDeclaration { typename = typename'
                                         , type' = EnumType members } = do
    let className = toClassName' typename'
        memberNames = T.intercalate
            "\n    "
            [ [qq|{toAttributeName' memberName} = '{toSnakeCaseText bn}'|]
            | EnumMember memberName@(Name _ bn) _ <- toList members
            ]
    insertStandardImport "enum"
    return [qq|
class $className(enum.Enum):
    # TODO: docstring

    $memberNames

    def __nirum_serialize__(self) -> str:
        return self.value

    @classmethod
    def __nirum_deserialize__(cls: type, value: str) -> '{className}':
        return cls(value.replace('-', '_'))  # FIXME: validate input
    |]
compileTypeDeclaration src TypeDeclaration { typename = typename'
                                           , type' = RecordType fields } = do
    typeExprCodes <- mapM (compileTypeExpression src)
        [typeExpr | (Field _ typeExpr _) <- toList fields]
    let className = toClassName' typename'
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
    insertStandardImport "typing"
    insertThirdPartyImports [ ("nirum.validate", ["validate_record_type"])
                            , ("nirum.serialize", ["serialize_record_type"])
                            , ("nirum.deserialize", ["deserialize_record_type"])
                            , ("nirum.constructs", ["name_dict_type"])
                            ]
    return [qq|
class $className:
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
compileTypeDeclaration src TypeDeclaration { typename = typename'
                                           , type' = UnionType tags } = do
    fieldCodes <- mapM (uncurry (compileUnionTag src typename')) tagNameNFields
    let className = toClassName' typename'
        fieldCodes' = T.intercalate "\n\n" fieldCodes
        enumMembers = toIndentedCodes
            (\(t, b) -> [qq|$t = '{b}'|]) enumMembers' "\n        "
    insertStandardImport "typing"
    insertStandardImport "enum"
    insertThirdPartyImports [ ("nirum.serialize", ["serialize_union_type"])
                            , ("nirum.deserialize", ["deserialize_union_type"])
                            , ("nirum.constructs", ["name_dict_type"])
                            ]
    return [qq|
class $className:

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
compileTypeDeclaration src ServiceDeclaration { serviceName = name
                                              , service = Service methods } = do
    let methods' = toList methods
    methodMetadata <- mapM compileMethodMetadata methods'
    let methodMetadata' = commaNl methodMetadata
    dummyMethods <- mapM compileMethod methods'
    clientMethods <- mapM compileClientMethod methods'
    let dummyMethods' = T.intercalate "\n\n" dummyMethods
        clientMethods' = T.intercalate "\n\n" clientMethods
    insertStandardImport "urllib.request"
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
    className = toClassName' name
    commaNl :: [T.Text] -> T.Text
    commaNl = T.intercalate ",\n"
    compileMethod :: Method -> CodeGen Code
    compileMethod (Method mName params rtype _etype _anno) = do
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
        params' <- mapM compileParameter $ toList params
        rtypeExpr <- compileTypeExpression src rtype
        payloadArguments <- mapM compileClientPayload $ toList params
        return [qq|
    def {clientMethodName'}(self, {commaNl params'}) -> $rtypeExpr:
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

compileTypeDeclaration _ Import { } =
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
                      -> (Int, Int)       -- | Python version already stasified
                      -> T.Text           -- | PyPI package name
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

compileModule :: Source -> Either CompileError (InstallRequires, Code)
compileModule source =
    case runCodeGen code' emptyContext of
        Left errMsg -> Left errMsg
        Right (code, context) -> codeWithDeps context $ [qq|
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
    codeWithDeps :: CodeGenContext -> Code -> Either CompileError (InstallRequires, Code)
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
compilePackageMetadata package (InstallRequires deps optDeps) = [qq|
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

# TODO: description, long_description, url, author, author_email, license,
#       keywords, classifiers
setup(
    name='{pName}',
    version='{pVersion}',
    packages=[$pPackages],
    provides=[$pPackages],
    requires=[$pInstallRequires],
    setup_requires=setup_requires,
    install_requires=install_requires,
    extras_require=extras_require,
)
|]
  where
    pName :: Code
    pName = "TestPackage"  -- FIXME
    pVersion :: Code
    pVersion = "0.1.0"  -- FIXME
    strings :: [Code] -> Code
    strings values = T.intercalate ", " . L.sort $ [[qq|'{v}'|] | v <- values]
    pPackages :: Code
    pPackages = strings $ map toImportPath $ M.keys $ modules package
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
    toFilename :: ModulePath -> FilePath
    toFilename mp =
        joinPath $ [ T.unpack (toAttributeName i)
                   | i <- toList mp
                   ] ++ ["__init__.py"]
    initFiles :: [(FilePath, Either CompileError Code)]
    initFiles = [ (toFilename mp', Right "")
                | mp <- M.keys (modules package)
                , mp' <- S.elems (ancestors mp)
                ]
    modules' :: [(FilePath, Either CompileError (InstallRequires, Code))]
    modules' =
        [ ( toFilename modulePath'
          , compileModule $ Source package boundModule
          )
        | (modulePath', _) <- M.assocs (modules package)
        , Just boundModule <- [resolveBoundModule modulePath' package]
        ]
    installRequires :: InstallRequires
    installRequires = foldl unionInstallRequires
                            (InstallRequires [] [])
                            [deps | (_, Right (deps, _)) <- modules']
