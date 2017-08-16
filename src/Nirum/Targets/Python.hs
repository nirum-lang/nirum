{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, OverloadedLists,
             QuasiQuotes, TypeFamilies, TypeSynonymInstances,
             MultiParamTypeClasses #-}
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
                            , Python (Python)
                            , PythonVersion ( Python2
                                            , Python3
                                            )
                            , RenameMap
                            , addDependency
                            , addOptionalDependency
                            , compileModule
                            , compilePrimitiveType
                            , compileTypeDeclaration
                            , compileTypeExpression
                            , empty
                            , insertLocalImport
                            , insertStandardImport
                            , insertThirdPartyImports
                            , insertThirdPartyImportsA
                            , localImportsMap
                            , minimumRuntime
                            , parseModulePath
                            , renameModulePath
                            , runCodeGen
                            , stringLiteral
                            , toAttributeName
                            , toClassName
                            , toImportPath
                            , toImportPaths
                            , toNamePair
                            , unionInstallRequires
                            ) where

import Control.Monad (forM)
import qualified Control.Monad.State as ST
import qualified Data.List as L
import Data.Maybe (catMaybes, fromMaybe)
import Data.Typeable (Typeable)
import GHC.Exts (IsList (toList))
import Text.Printf (printf)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.SemVer as SV
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Function (on)
import System.FilePath (joinPath)
import qualified Text.Email.Validate as E
import Text.InterpolatedString.Perl6 (q, qq)

import qualified Nirum.CodeGen as C
import Nirum.CodeGen (Failure)
import qualified Nirum.Constructs.Annotation as A
import qualified Nirum.Constructs.DeclarationSet as DS
import qualified Nirum.Constructs.Identifier as I
import Nirum.Constructs.Declaration (Documented (docsBlock))
import Nirum.Constructs.ModulePath ( ModulePath
                                   , fromIdentifiers
                                   , hierarchy
                                   , hierarchies
                                   , replacePrefix
                                   )
import Nirum.Constructs.Name (Name (Name))
import qualified Nirum.Constructs.Name as N
import Nirum.Constructs.Service ( Method ( Method
                                         , errorType
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
import Nirum.Docs.ReStructuredText (ReStructuredText, render)
import Nirum.Package ( BoundModule
                     , Package (Package, metadata, modules)
                     , TypeLookup (Imported, Local, Missing)
                     , lookupType
                     , resolveBoundModule
                     , types
                     )
import Nirum.Package.Metadata ( Author (Author, name, email)
                              , Metadata (Metadata, authors, target, version)
                              , MetadataError ( FieldError
                                              , FieldTypeError
                                              , FieldValueError
                                              )
                              , Node (VString)
                              , Target ( CompileError
                                       , CompileResult
                                       , compilePackage
                                       , parseTarget
                                       , showCompileError
                                       , targetName
                                       , toByteString
                                       )
                              , fieldType
                              , stringField
                              , tableField
                              , versionField
                              )
import qualified Nirum.Package.ModuleSet as MS

minimumRuntime :: SV.Version
minimumRuntime = SV.version 0 6 0 [] []

data Python = Python { packageName :: T.Text
                     , minimumRuntimeVersion :: SV.Version
                     , renames :: RenameMap
                     } deriving (Eq, Ord, Show, Typeable)

type RenameMap = M.Map ModulePath ModulePath
type Package' = Package Python
type CompileError' = T.Text

data PythonVersion = Python2
                   | Python3
                   deriving (Eq, Ord, Show)

data Source = Source { sourcePackage :: Package'
                     , sourceModule :: BoundModule Python
                     } deriving (Eq, Ord, Show)

type Code = T.Text

instance Failure CodeGenContext CompileError' where
    fromString = return . T.pack

data CodeGenContext
    = CodeGenContext { standardImports :: S.Set T.Text
                     , thirdPartyImports :: M.Map T.Text (M.Map T.Text T.Text)
                     , localImports :: M.Map T.Text (S.Set T.Text)
                     , pythonVersion :: PythonVersion
                     }
    deriving (Eq, Ord, Show)

localImportsMap :: CodeGenContext -> M.Map T.Text (M.Map T.Text T.Text)
localImportsMap CodeGenContext { localImports = imports } =
    M.map (M.fromSet id) imports

sourceDirectory :: PythonVersion -> T.Text
sourceDirectory Python2 = "src-py2"
sourceDirectory Python3 = "src"

empty :: PythonVersion -> CodeGenContext
empty pythonVersion' = CodeGenContext { standardImports = []
                                      , thirdPartyImports = []
                                      , localImports = []
                                      , pythonVersion = pythonVersion'
                                      }

type CodeGen = C.CodeGen CodeGenContext CompileError'

runCodeGen :: CodeGen a
           -> CodeGenContext
           -> (Either CompileError' a, CodeGenContext)
runCodeGen = C.runCodeGen

insertStandardImport :: T.Text -> CodeGen ()
insertStandardImport module' = ST.modify insert'
  where
    insert' c@CodeGenContext { standardImports = si } =
        c { standardImports = S.insert module' si }

insertThirdPartyImports :: [(T.Text, S.Set T.Text)] -> CodeGen ()
insertThirdPartyImports imports =
    insertThirdPartyImportsA [ (from, M.fromSet id objects)
                             | (from, objects) <- imports
                             ]

insertThirdPartyImportsA :: [(T.Text, M.Map T.Text T.Text)] -> CodeGen ()
insertThirdPartyImportsA imports =
    ST.modify insert'
  where
    insert' c@CodeGenContext { thirdPartyImports = ti } =
        c { thirdPartyImports = L.foldl (M.unionWith M.union) ti importList }
    importList :: [M.Map T.Text (M.Map T.Text T.Text)]
    importList = [ M.singleton from objects
                 | (from, objects) <- imports
                 ]

insertLocalImport :: T.Text -> T.Text -> CodeGen ()
insertLocalImport module' object = ST.modify insert'
  where
    insert' c@CodeGenContext { localImports = li } =
        c { localImports = M.insertWith S.union module' [object] li }


importTypingForPython3 :: CodeGen ()
importTypingForPython3 = do
    pyVer <- getPythonVersion
    case pyVer of
        Python2 -> return ()
        Python3 -> insertStandardImport "typing"

insertEnumImport :: CodeGen ()
insertEnumImport = insertStandardImport "enum"

getPythonVersion :: CodeGen PythonVersion
getPythonVersion = fmap pythonVersion ST.get

renameModulePath :: RenameMap -> ModulePath -> ModulePath
renameModulePath renameMap path' =
    rename (M.toDescList renameMap)
    -- longest paths should be processed first
  where
    rename :: [(ModulePath, ModulePath)] -> ModulePath
    rename ((from, to) : xs) = let r = replacePrefix from to path'
                               in if r == path'
                                  then rename xs
                                  else r
    rename [] = path'

renameMP :: Python -> ModulePath -> ModulePath
renameMP Python { renames = table } = renameModulePath table

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

toClassName :: I.Identifier -> T.Text
toClassName identifier =
    if className `S.member` keywords then className `T.snoc` '_' else className
  where
    className :: T.Text
    className = I.toPascalCaseText identifier

toClassName' :: Name -> T.Text
toClassName' = toClassName . N.facialName

toAttributeName :: I.Identifier -> T.Text
toAttributeName identifier =
    if attrName `S.member` keywords then attrName `T.snoc` '_' else attrName
  where
    attrName :: T.Text
    attrName = I.toSnakeCaseText identifier

toAttributeName' :: Name -> T.Text
toAttributeName' = toAttributeName . N.facialName

toImportPath' :: ModulePath -> T.Text
toImportPath' = T.intercalate "." . map toAttributeName . toList

toImportPath :: Python -> ModulePath -> T.Text
toImportPath target' = toImportPath' . renameMP target'

toImportPaths :: Python -> S.Set ModulePath -> [T.Text]
toImportPaths target' paths =
    S.toAscList $ S.map toImportPath' $ hierarchies renamedPaths
  where
    renamedPaths :: S.Set ModulePath
    renamedPaths = S.map (renameMP target') paths

toNamePair :: Name -> T.Text
toNamePair (Name f b) = [qq|('{toAttributeName f}', '{I.toSnakeCaseText b}')|]

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

compileParameters :: (ParameterName -> ParameterType -> Code)
                  -> [(T.Text, Code, Bool)]
                  -> Code
compileParameters gen nameTypeTriples =
    toIndentedCodes
        (\ (n, t, o) -> gen n t `T.append` if o then "=None" else "")
        nameTypeTriples ", "

compileFieldInitializers :: DS.DeclarationSet Field -> Int -> CodeGen Code
compileFieldInitializers fields depth = do
    initializers <- forM (toList fields) compileFieldInitializer
    return $ T.intercalate indentSpaces initializers
  where
    indentSpaces :: T.Text
    indentSpaces = "\n" `T.append` T.replicate depth "    "
    compileFieldInitializer :: Field -> CodeGen Code
    compileFieldInitializer (Field fieldName' fieldType' _) =
        case fieldType' of
            SetModifier _ ->
                return [qq|self.$attributeName = frozenset($attributeName)|]
            ListModifier _ -> do
                insertThirdPartyImportsA [ ( "nirum.datastructures"
                                           , [("list_type", "List")]
                                           )
                                         ]
                return [qq|self.$attributeName = list_type($attributeName)|]
            _ -> return [qq|self.$attributeName = $attributeName|]
      where
        attributeName :: Code
        attributeName = toAttributeName' fieldName'

compileDocs :: Documented a => a -> Maybe ReStructuredText
compileDocs = fmap render . docsBlock

quoteDocstring :: ReStructuredText -> Code
quoteDocstring rst = T.concat ["r'''", rst, "\n'''\n"]

compileDocstring' :: Documented a => Code -> a -> [ReStructuredText] -> Code
compileDocstring' indentSpace d extra =
    case (compileDocs d, extra) of
        (Nothing, []) -> "\n"
        (result, extra') -> indent indentSpace $ quoteDocstring $
            T.append (fromMaybe "" result) $
                     T.concat ['\n' `T.cons` e `T.snoc` '\n' | e <- extra']

compileDocstring :: Documented a => Code -> a -> Code
compileDocstring indentSpace d = compileDocstring' indentSpace d []

compileDocstringWithFields :: Documented a
                           => Code -> a -> DS.DeclarationSet Field -> Code
compileDocstringWithFields indentSpace decl fields =
    compileDocstring' indentSpace decl extra
  where
    extra :: [ReStructuredText]
    extra =
        [ case compileDocs f of
              Nothing -> T.concat [ ".. attribute:: "
                                  , toAttributeName' n
                                  , "\n"
                                  ]
              Just docs' -> T.concat [ ".. attribute:: "
                                     , toAttributeName' n
                                     , "\n\n"
                                     , indent "   " docs'
                                     ]
        | f@(Field n _ _) <- toList fields
        ]

compileDocsComment :: Documented a => Code -> a -> Code
compileDocsComment indentSpace d =
    case compileDocs d of
        Nothing -> "\n"
        Just rst -> indent (indentSpace `T.append` "#: ") rst

indent :: Code -> Code -> Code
indent space =
    T.intercalate "\n" . map indentLn . T.lines
  where
    indentLn :: Code -> Code
    indentLn line
      | T.null line = T.empty
      | otherwise = space `T.append` line

typeReprCompiler :: CodeGen (Code -> Code)
typeReprCompiler = do
    ver <- getPythonVersion
    case ver of
        Python2 -> return $ \ t -> [qq|($t.__module__ + '.' + $t.__name__)|]
        Python3 -> do
            insertStandardImport "typing"
            return $ \ t -> [qq|typing._type_repr($t)|]

type ParameterName = Code
type ParameterType = Code
type ReturnType = Code

parameterCompiler :: CodeGen (ParameterName -> ParameterType -> Code)
parameterCompiler = do
    ver <- getPythonVersion
    return $ \ n t -> case ver of
                          Python2 -> n
                          Python3 -> [qq|$n: '{t}'|]

returnCompiler :: CodeGen (ReturnType -> Code)
returnCompiler = do
    ver <- getPythonVersion
    return $ \ r ->
        case (ver, r) of
            (Python2, _) -> ""
            (Python3, "None") -> [qq| -> None|]
            (Python3, _) -> [qq| -> '{r}'|]


compileUnionTag :: Source -> Name -> Tag -> CodeGen Code
compileUnionTag source parentname d@(Tag typename' fields _) = do
    typeExprCodes <- mapM (compileTypeExpression source)
        [typeExpr | (Field _ typeExpr _) <- toList fields]
    let optionFlags = [ case typeExpr of
                            OptionModifier _ -> True
                            _ -> False
                      | (Field _ typeExpr _) <- toList fields
                      ]
        className = toClassName' typename'
        tagNames = map (toAttributeName' . fieldName) (toList fields)
        getOptFlag :: (T.Text, Code, Bool) -> Bool
        getOptFlag (_, _, flag) = flag
        nameTypeTriples = L.sortBy (compare `on` getOptFlag)
                                   (zip3 tagNames typeExprCodes optionFlags)
        slotTypes = toIndentedCodes
            (\ (n, t, _) -> [qq|('{n}', {t})|]) nameTypeTriples ",\n        "
        slots = if length tagNames == 1
                then [qq|'{head tagNames}'|] `T.snoc` ','
                else toIndentedCodes (\ n -> [qq|'{n}'|]) tagNames ",\n        "
        hashTuple = if null tagNames
            then "self.__nirum_tag__"
            else [qq|({attributes},)|] :: T.Text
          where
            attributes :: T.Text
            attributes = toIndentedCodes (\ n -> [qq|self.{n}|]) tagNames ", "
        nameMaps = toIndentedCodes
            toNamePair
            (map fieldName $ toList fields)
            ",\n        "
        parentClass = toClassName' parentname
    insertThirdPartyImportsA
        [ ("nirum.validate", [("validate_union_type", "validate_union_type")])
        , ("nirum.constructs", [("name_dict_type", "NameDict")])
        ]
    typeRepr <- typeReprCompiler
    arg <- parameterCompiler
    ret <- returnCompiler
    pyVer <- getPythonVersion
    initializers <- compileFieldInitializers fields $ case pyVer of
        Python3 -> 2
        Python2 -> 3
    let initParams = compileParameters arg nameTypeTriples
        inits = case pyVer of
            Python2 -> [qq|
    def __init__(self, **kwargs):
        def __init__($initParams):
            $initializers
            pass
        __init__(**kwargs)
        validate_union_type(self)
            |]
            Python3 -> [qq|
    def __init__(self{ if null nameTypeTriples
                         then T.empty
                         else ", *, " `T.append` initParams }) -> None:
        $initializers
        validate_union_type(self)
            |]
    return [qq|
class $className($parentClass):
{compileDocstringWithFields "    " d fields}
    __slots__ = (
        $slots
    )
    __nirum_tag__ = $parentClass.Tag.{toAttributeName' typename'}
    __nirum_tag_names__ = name_dict_type([
        $nameMaps
    ])

    @staticmethod
    def __nirum_tag_types__():
        return [$slotTypes]

    { inits :: T.Text }

    def __repr__(self){ ret "str" }:
        return '\{0\}(\{1\})'.format(
            {typeRepr "type(self)"},
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
        (Int64, Python2) -> do
            insertStandardImport "numbers"
            return "numbers.Integral"
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
compileTypeExpression Source { sourcePackage = Package { metadata = meta }
                             , sourceModule = boundModule
                             }
                      (TypeIdentifier i) =
    case lookupType i boundModule of
        Missing -> fail $ "undefined identifier: " ++ I.toString i
        Imported _ (PrimitiveType p _) -> compilePrimitiveType p
        Imported m _ -> do
            insertThirdPartyImports [(toImportPath target' m, [toClassName i])]
            return $ toClassName i
        Local _ -> return $ toClassName i
  where
    target' :: Python
    target' = target meta
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
compileTypeDeclaration _ TypeDeclaration { type' = PrimitiveType {} } =
    return ""  -- never used
compileTypeDeclaration src d@TypeDeclaration { typename = typename'
                                             , type' = Alias ctype
                                             } = do
    ctypeExpr <- compileTypeExpression src ctype
    return [qq|
$docsComment
{toClassName' typename'} = $ctypeExpr
    |]
  where
    docsComment :: Code
    docsComment =
        case compileDocs d of
            Nothing -> ""
            Just rst -> indent "#: " rst
compileTypeDeclaration src d@TypeDeclaration { typename = typename'
                                             , type' = UnboxedType itype
                                             } = do
    let className = toClassName' typename'
    itypeExpr <- compileTypeExpression src itype
    insertThirdPartyImports [ ("nirum.validate", ["validate_boxed_type"])
                            , ("nirum.serialize", ["serialize_boxed_type"])
                            , ("nirum.deserialize", ["deserialize_boxed_type"])
                            ]
    arg <- parameterCompiler
    typeRepr <- typeReprCompiler
    ret <- returnCompiler
    return [qq|
class $className(object):
{compileDocstring "    " d}
    @staticmethod
    def __nirum_get_inner_type__():
        return $itypeExpr

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
    ){ ret className }:
        return deserialize_boxed_type(cls, value)

    def __repr__(self){ ret "str" }:
        return '\{0\}(\{1!r\})'.format(
            {typeRepr "type(self)"}, self.value
        )

    def __hash__(self){ ret "int" }:
        return hash(self.value)
|]
compileTypeDeclaration _ d@TypeDeclaration { typename = typename'
                                           , type' = EnumType members
                                           } = do
    let className = toClassName' typename'
        memberNames = T.intercalate
            "\n"
            [ T.concat [ compileDocsComment "    " m
                       , "\n    "
                       , toAttributeName' memberName
                       , " = '"
                       , I.toSnakeCaseText bn
                       , "'"
                       ]
            | m@(EnumMember memberName@(Name _ bn) _) <- toList members
            ]
    insertEnumImport
    arg <- parameterCompiler
    ret <- returnCompiler
    return [qq|
class $className(enum.Enum):
{compileDocstring "    " d}

$memberNames

    def __nirum_serialize__(self){ ret "str" }:
        return self.value

    @classmethod
    def __nirum_deserialize__(
        {arg "cls" "type"},
        {arg "value" "str"}
    ){ ret className }:
        return cls(value.replace('-', '_'))  # FIXME: validate input
|]
compileTypeDeclaration src d@TypeDeclaration { typename = typename'
                                             , type' = RecordType fields
                                             } = do
    let className = toClassName' typename'
        fieldList = toList fields
    typeExprCodes <- mapM (compileTypeExpression src)
        [typeExpr | (Field _ typeExpr _) <- fieldList]
    let optionFlags = [ case typeExpr of
                            OptionModifier _ -> True
                            _ -> False
                      | (Field _ typeExpr _) <- fieldList
                      ]
        fieldNames = map toAttributeName' [ name'
                                          | (Field name' _ _) <- fieldList
                                          ]
        getOptFlag :: (T.Text, Code, Bool) -> Bool
        getOptFlag (_, _, flag) = flag
        nameTypeTriples = L.sortBy (compare `on` getOptFlag)
                                   (zip3 fieldNames typeExprCodes optionFlags)
        slotTypes = toIndentedCodes
            (\ (n, t, _) -> [qq|'{n}': {t}|]) nameTypeTriples ",\n        "
        slots = toIndentedCodes (\ n -> [qq|'{n}'|]) fieldNames ",\n        "
        nameMaps = toIndentedCodes
            toNamePair
            (map fieldName $ toList fields)
            ",\n        "
        hashText = toIndentedCodes (\ n -> [qq|self.{n}|]) fieldNames ", "
    importTypingForPython3
    insertThirdPartyImports [ ("nirum.validate", ["validate_record_type"])
                            , ("nirum.serialize", ["serialize_record_type"])
                            , ("nirum.deserialize", ["deserialize_record_type"])
                            ]
    insertThirdPartyImportsA [ ( "nirum.constructs"
                               , [("name_dict_type", "NameDict")]
                               )
                             ]
    arg <- parameterCompiler
    ret <- returnCompiler
    typeRepr <- typeReprCompiler
    pyVer <- getPythonVersion
    initializers <- compileFieldInitializers fields $ case pyVer of
        Python3 -> 2
        Python2 -> 3
    let initParams = compileParameters arg nameTypeTriples
        inits = case pyVer of
            Python2 -> [qq|
    def __init__(self, **kwargs):
        def __init__($initParams):
            $initializers
            pass
        __init__(**kwargs)
        validate_record_type(self)
            |]
            Python3 -> [qq|
    def __init__(self{ if null nameTypeTriples
                         then T.empty
                         else ", *, " `T.append` initParams }) -> None:
        $initializers
        validate_record_type(self)
            |]
    let clsType = arg "cls" "type"
    return [qq|
class $className(object):
{compileDocstringWithFields "    " d fields}
    __slots__ = (
        $slots,
    )
    __nirum_record_behind_name__ = (
        '{I.toSnakeCaseText $ N.behindName typename'}'
    )
    __nirum_field_names__ = name_dict_type([$nameMaps])

    @staticmethod
    def __nirum_field_types__():
        return \{$slotTypes\}

    {inits :: T.Text}

    def __repr__(self){ret "bool"}:
        return '\{0\}(\{1\})'.format(
            {typeRepr "type(self)"},
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
    def __nirum_deserialize__($clsType, value){ ret className }:
        return deserialize_record_type(cls, value)

    def __hash__(self){ret "int"}:
        return hash(($hashText,))
|]
compileTypeDeclaration src
                       d@TypeDeclaration { typename = typename'
                                         , type' = UnionType tags
                                         , typeAnnotations = annotations
                                         } = do
    tagCodes <- mapM (compileUnionTag src typename') $ toList tags
    let className = toClassName' typename'
        tagCodes' = T.intercalate "\n\n" tagCodes
        enumMembers = toIndentedCodes
            (\ (t, b) -> [qq|$t = '{b}'|]) enumMembers' "\n        "
    importTypingForPython3
    insertEnumImport
    insertThirdPartyImports [ ("nirum.serialize", ["serialize_union_type"])
                            , ("nirum.deserialize", ["deserialize_union_type"])
                            ]
    insertThirdPartyImportsA [ ( "nirum.constructs"
                               , [("name_dict_type", "NameDict")]
                               )
                             ]
    typeRepr <- typeReprCompiler
    ret <- returnCompiler
    arg <- parameterCompiler
    return [qq|
class $className({T.intercalate "," $ compileExtendClasses annotations}):
{compileDocstring "    " d}

    __nirum_union_behind_name__ = '{I.toSnakeCaseText $ N.behindName typename'}'
    __nirum_field_names__ = name_dict_type([$nameMaps])

    class Tag(enum.Enum):
        $enumMembers

    def __init__(self, *args, **kwargs):
        raise NotImplementedError(
            "\{0\} cannot be instantiated "
            "since it is an abstract class.  Instantiate a concrete subtype "
            "of it instead.".format({typeRepr "type(self)"})
        )

    def __nirum_serialize__(self){ ret "typing.Mapping[str, typing.Any]" }:
        return serialize_union_type(self)

    @classmethod
    def __nirum_deserialize__(
        {arg "cls" "type"}, value
    ){ ret className }:
        return deserialize_union_type(cls, value)


$tagCodes'
            |]
  where
    enumMembers' :: [(T.Text, T.Text)]
    enumMembers' = [ ( toAttributeName' tagName
                     , I.toSnakeCaseText $ N.behindName tagName
                     )
                   | (Tag tagName _ _) <- toList tags
                   ]
    nameMaps :: T.Text
    nameMaps = toIndentedCodes
        toNamePair
        [name' | Tag name' _ _ <- toList tags]
        ",\n        "
    compileExtendClasses :: A.AnnotationSet -> [Code]
    compileExtendClasses annotations' =
        if null extendClasses
            then ["object"]
            else extendClasses
      where
        extendsClassMap :: M.Map I.Identifier Code
        extendsClassMap = [("error", "Exception")]
        extendClasses :: [Code]
        extendClasses = catMaybes
            [ M.lookup annotationName extendsClassMap
            | (A.Annotation annotationName _) <- A.toList annotations'
            ]
compileTypeDeclaration
    src@Source { sourcePackage = Package { metadata = metadata' } }
    d@ServiceDeclaration { serviceName = name'
                         , service = Service methods
                         } = do
    let methods' = toList methods
    methodMetadata <- mapM compileMethodMetadata methods'
    let methodMetadata' = commaNl methodMetadata
    dummyMethods <- mapM compileMethod methods'
    clientMethods <- mapM compileClientMethod methods'
    methodErrorTypes <- mapM compileErrorType methods'
    let dummyMethods' = T.intercalate "\n\n" dummyMethods
        clientMethods' = T.intercalate "\n\n" clientMethods
        methodErrorTypes' =
            T.intercalate "," $ catMaybes methodErrorTypes
    param <- parameterCompiler
    ret <- returnCompiler
    insertStandardImport "json"
    insertThirdPartyImports [ ("nirum.deserialize", ["deserialize_meta"])
                            , ("nirum.serialize", ["serialize_meta"])
                            ]
    insertThirdPartyImportsA
        [ ("nirum.constructs", [("name_dict_type", "NameDict")])
        , ("nirum.service", [("service_type", "Service")])
        , ("nirum.transport", [("transport_type", "Transport")])
        ]
    return [qq|
class $className(service_type):
{compileDocstring "    " d}
    __nirum_schema_version__ = \'{SV.toText $ version metadata'}\'
    __nirum_service_methods__ = \{
        {methodMetadata'}
    \}
    __nirum_method_names__ = name_dict_type([
        $methodNameMap
    ])

    @staticmethod
    def __nirum_method_error_types__(k, d=None):
        return dict([
            $methodErrorTypes'
        ]).get(k, d)

    {dummyMethods'}


# FIXME client MUST be generated & saved on diffrent module
#       where service isn't included.
class {className}_Client($className):
    """The client object of :class:`{className}`."""

    def __init__(self,
                 { param "transport" "transport_type" }){ ret "None" }:
        if not isinstance(transport, transport_type):
            raise TypeError(
                'expected an instance of \{0.__module__\}.\{0.__name__\}, not '
                '\{1!r\}'.format(transport_type, transport)
            )
        self.__nirum_transport__ = transport  # type: transport_type

    {clientMethods'}
|]
  where
    className :: T.Text
    className = toClassName' name'
    commaNl :: [T.Text] -> T.Text
    commaNl = T.intercalate ",\n"
    compileErrorType :: Method -> CodeGen (Maybe Code)
    compileErrorType (Method mn _ _ me _) =
        case me of
            Just errorTypeExpression -> do
                et <- compileTypeExpression src errorTypeExpression
                return $ Just [qq|('{toAttributeName' mn}', $et)|]
            Nothing -> return Nothing
    compileMethod :: Method -> CodeGen Code
    compileMethod m@(Method mName params rtype _etype _anno) = do
        let mName' = toAttributeName' mName
        params' <- mapM compileMethodParameter $ toList params
        let paramDocs = [ T.concat [ ":param "
                                   , toAttributeName' pName
                                   , maybe "" (T.append ": ") $ compileDocs p
                                   -- TODO: types
                                   ]
                        | p@(Parameter pName _ _) <- toList params
                        ]
        rtypeExpr <- compileTypeExpression src rtype
        ret <- returnCompiler
        return [qq|
    def {mName'}(self, {commaNl params'}){ ret rtypeExpr }:
{compileDocstring' "        " m paramDocs}
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
        insertThirdPartyImportsA
            [("nirum.constructs", [("name_dict_type", "NameDict")])]
        return [qq|'{toAttributeName' mName}': \{
            '_v': 2,
            '_return': lambda: $rtypeExpr,
            '_names': name_dict_type([{paramNameMap params'}]),
            {paramMetadata'}
        \}|]
    compileParameterMetadata :: Parameter -> CodeGen Code
    compileParameterMetadata (Parameter pName pType _) = do
        let pName' = toAttributeName' pName
        pTypeExpr <- compileTypeExpression src pType
        return [qq|'{pName'}': lambda: $pTypeExpr|]
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
        return [qq|'{I.toSnakeCaseText $ N.behindName pName}':
                   serialize_meta({pName'})|]
    compileClientMethod :: Method -> CodeGen Code
    compileClientMethod Method { methodName = mName
                               , parameters = params
                               , returnType = rtype
                               , errorType = etypeM
                               } = do
        let clientMethodName' = toAttributeName' mName
        params' <- mapM compileMethodParameter $ toList params
        rtypeExpr <- compileTypeExpression src rtype
        errorCode <- case etypeM of
             Just e -> do
                e' <- compileTypeExpression src e
                return $ "result_type = " `T.append` e'
             Nothing ->
                return "raise UnexpectedNirumResponseError(serialized)"
        payloadArguments <- mapM compileClientPayload $ toList params
        ret <- returnCompiler
        return [qq|
    def {clientMethodName'}(self, {commaNl params'}){ret rtypeExpr}:
        successful, serialized = self.__nirum_transport__(
            '{I.toSnakeCaseText $ N.behindName mName}',
            payload=\{{commaNl payloadArguments}\},
            # FIXME Give annotations.
            service_annotations=\{\},
            method_annotations=\{\},
            parameter_annotations=\{\}
        )
        if successful:
            result_type = $rtypeExpr
        else:
            $errorCode
        result = deserialize_meta(result_type, serialized)
        if successful:
            return result
        raise result
|]

compileTypeDeclaration _ Import {} =
    return ""  -- Nothing to compile

compileModuleBody :: Source -> CodeGen Code
compileModuleBody src@Source { sourceModule = boundModule } = do
    let types' = types boundModule
    typeCodes <- mapM (compileTypeDeclaration src) $ toList types'
    return $ T.intercalate "\n\n" typeCodes

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
              -> Either CompileError' (InstallRequires, Code)
compileModule pythonVersion' source =
    case runCodeGen code' $ empty pythonVersion' of
        (Left errMsg, _) -> Left errMsg
        (Right code, context) -> codeWithDeps context $
            [qq|# -*- coding: utf-8 -*-
{compileDocstring "" $ sourceModule source}
{imports $ standardImports context}

{fromImports $ localImportsMap context}

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
    fromImports :: M.Map T.Text (M.Map T.Text T.Text) -> T.Text
    fromImports importMap =
        T.intercalate "\n"
            [ [qq|from $from import {T.intercalate ", " $ map importString
                                                        $ M.assocs objects}|]
            | (from, objects) <- M.assocs importMap
            ]
    importString :: (T.Text, T.Text) -> T.Text
    importString (alias, var)
      | var == alias = alias
      | otherwise = [qq|$var as $alias|]
    has :: S.Set T.Text -> T.Text -> Bool
    has set module' = module' `S.member` set ||
                      any (T.isPrefixOf $ module' `T.snoc` '.') set
    require :: T.Text -> T.Text -> S.Set T.Text -> S.Set T.Text
    require pkg module' set =
        if set `has` module' then S.singleton pkg else S.empty
    codeWithDeps :: CodeGenContext
                 -> Code
                 -> Either CompileError' (InstallRequires, Code)
    codeWithDeps context c = Right (InstallRequires deps optDeps, c)
      where
        deps :: S.Set T.Text
        deps = require "nirum" "nirum" $ M.keysSet $ thirdPartyImports context
        optDeps :: M.Map (Int, Int) (S.Set T.Text)
        optDeps =
            [ ((3, 4), require "enum34" "enum" $ standardImports context)
            , ((3, 5), require "typing" "typing" $ standardImports context)
            ]

compilePackageMetadata :: Package' -> InstallRequires -> Code
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


SOURCE_ROOT = '{sourceDirectory Python3}'

if sys.version_info < (3, 0):
    SOURCE_ROOT = '{sourceDirectory Python2}'

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
    requires=[$pRequires],
    setup_requires=setup_requires,
    install_requires=install_requires,
    extras_require=extras_require,
)
|]
  where
    target' :: Python
    target' = target metadata'
    csStrings :: [T.Text] -> T.Text
    csStrings [] = "None"
    csStrings s = stringLiteral $ T.intercalate ", " s
    pName :: Code
    pName = packageName $ target metadata'
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
    pPackages = strings $ toImportPaths target' $ MS.keysSet $ modules package
    runtimeVer :: SV.Version
    runtimeVer = minimumRuntimeVersion $ target metadata'
    pRequires :: Code
    pRequires = strings $ S.toList deps
    pInstallRequires :: Code
    pInstallRequires = strings
        [ case p of
              "nirum" -> [qq|nirum >= {SV.toText runtimeVer}|]
              p' -> p'
        | p <- S.toList deps
        ]
    pPolyfillRequires :: Code
    pPolyfillRequires = T.intercalate ", "
        [ [qq|($major, $minor): [{strings $ S.toList deps'}]|]
        | ((major, minor), deps') <- M.toList optDeps
        ]

manifestIn :: Code
manifestIn = [q|recursive-include src *.py
recursive-include src-py2 *.py
|]

compilePackage' :: Package'
                -> M.Map FilePath (Either CompileError' Code)
compilePackage' package@Package { metadata = Metadata { target = target' } } =
    M.fromList $
        initFiles ++
        [ ( f
          , case cd of
                Left e -> Left e
                Right (_, cd') -> Right cd'
          )
        | (f, cd) <- modules'
        ] ++
        [ ("setup.py", Right $ compilePackageMetadata package installRequires)
        , ("MANIFEST.in", Right manifestIn)
        ]
  where
    toPythonFilename :: ModulePath -> [FilePath]
    toPythonFilename mp = [ T.unpack (toAttributeName i)
                          | i <- toList $ renameMP target' mp
                          ] ++ ["__init__.py"]
    versions :: [PythonVersion]
    versions = [Python2, Python3]
    toFilename :: T.Text -> ModulePath -> FilePath
    toFilename sourceRootDirectory mp =
        joinPath $ T.unpack sourceRootDirectory : toPythonFilename mp
    initFiles :: [(FilePath, Either CompileError' Code)]
    initFiles = [ (toFilename (sourceDirectory ver) mp', Right "")
                | mp <- MS.keys (modules package)
                , mp' <- S.elems (hierarchy mp)
                , ver <- versions
                ]
    modules' :: [(FilePath, Either CompileError' (InstallRequires, Code))]
    modules' =
        [ ( toFilename (sourceDirectory ver) modulePath'
          , compileModule ver $ Source package boundModule
          )
        | (modulePath', _) <- MS.toAscList (modules package)
        , Just boundModule <- [resolveBoundModule modulePath' package]
        , ver <- versions
        ]
    installRequires :: InstallRequires
    installRequires = foldl unionInstallRequires
                            (InstallRequires [] [])
                            [deps | (_, Right (deps, _)) <- modules']

parseModulePath :: T.Text -> Maybe ModulePath
parseModulePath string =
    mapM I.fromText identTexts >>= fromIdentifiers
  where
    identTexts :: [T.Text]
    identTexts = T.split (== '.') string

instance Target Python where
    type CompileResult Python = Code
    type CompileError Python = CompileError'
    targetName _ = "python"
    parseTarget table = do
        name' <- stringField "name" table
        minRuntime <- case versionField "minimum_runtime" table of
            Left (FieldError _) -> Right minimumRuntime
            otherwise' -> otherwise'
        renameTable <- case tableField "renames" table of
            Right t -> Right t
            Left (FieldError _) -> Right HM.empty
            otherwise' -> otherwise'
        renamePairs <- sequence
            [ case (parseModulePath k, v) of
                  (Just modulePath', VString v') -> case parseModulePath v' of
                      Just altPath -> Right (modulePath', altPath)
                      Nothing -> Left $ FieldValueError [qq|renames.$k|]
                          [qq|expected a module path, not "$v'"|]
                  (Nothing, _) -> Left $ FieldValueError [qq|renams.$k|]
                      [qq|expected a module path as a key, not "$k"|]
                  _ -> Left $ FieldTypeError [qq|renames.$k|] "string" $
                                             fieldType v
            | (k, v) <- HM.toList renameTable
            ]
        return Python { packageName = name'
                      , minimumRuntimeVersion = max minRuntime minimumRuntime
                      , renames = M.fromList renamePairs
                      }
    compilePackage = compilePackage'
    showCompileError _ e = e
    toByteString _ = encodeUtf8
