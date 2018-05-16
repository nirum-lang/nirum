{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Nirum.Targets.Python
    ( Python (..)
    , Source (..)
    , compileModule
    , compileTypeDeclaration
    , parseModulePath
    , toNamePair
    ) where

import Control.Monad (forM)
import qualified Data.List as L
import Data.Maybe (catMaybes, fromMaybe)
import GHC.Exts (IsList (toList))

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.SemVer as SV
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Function (on)
import System.FilePath (joinPath)
import Text.Blaze (Markup)
import Text.Blaze.Renderer.Text
import qualified Text.Email.Validate as E
import Text.Heterocephalus (compileText)
import Text.InterpolatedString.Perl6 (q, qq)

import qualified Nirum.Constructs.Annotation as A
import Nirum.Constructs.Annotation.Internal hiding (Text, annotations, name)
import qualified Nirum.Constructs.Annotation.Internal as AI
import qualified Nirum.Constructs.DeclarationSet as DS
import qualified Nirum.Constructs.Identifier as I
import Nirum.Constructs.Declaration (Documented (docsBlock))
import Nirum.Constructs.Module hiding (imports)
import Nirum.Constructs.ModulePath ( ModulePath
                                   , fromIdentifiers
                                   , hierarchy
                                   )
import Nirum.Constructs.Name (Name (Name))
import qualified Nirum.Constructs.Name as N
import Nirum.Constructs.Service ( Method ( Method
                                         , errorType
                                         , methodAnnotations
                                         , methodName
                                         , parameters
                                         , returnType
                                         )
                                , Parameter (Parameter)
                                , Service (Service)
                                )
import Nirum.Constructs.TypeDeclaration as TD
import Nirum.Constructs.TypeExpression hiding (type')
import Nirum.Docs.ReStructuredText (ReStructuredText, render)
import Nirum.Package hiding (target)
import Nirum.Package.Metadata ( Author (Author, name, email)
                              , Metadata ( authors
                                         , description
                                         , license
                                         , target
                                         , version
                                         )
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
                              , stringField
                              , tableField
                              , textArrayField
                              , versionField
                              )
import qualified Nirum.Package.ModuleSet as MS
import qualified Nirum.Package.Metadata as MD
import Nirum.Targets.Python.CodeGen
import Nirum.Targets.Python.Deserializers
import Nirum.Targets.Python.Serializers
import Nirum.Targets.Python.TypeExpression
import Nirum.Targets.Python.Validators
import Nirum.TypeInstance.BoundModule as BM

type Package' = Package Python
type CompileError' = Nirum.Targets.Python.CodeGen.CompileError

data Source = Source { sourcePackage :: Package'
                     , sourceModule :: BoundModule Python
                     } deriving (Eq, Ord, Show)

sourceImportPath :: Source -> T.Text
sourceImportPath (Source (Package MD.Metadata { MD.target = t } _) bm) =
    toImportPath t (BM.modulePath bm)

sourceDirectory :: PythonVersion -> T.Text
sourceDirectory Python2 = "src-py2"
sourceDirectory Python3 = "src"

thd3 :: (a, b, c) -> c
thd3 (_, _, v) = v

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

toEnumMemberName :: Name -> T.Text
toEnumMemberName name'
  | attributeName `elem` memberKeywords = attributeName `T.snoc` '_'
  | otherwise = attributeName
  where
    memberKeywords :: [T.Text]
    memberKeywords = ["mro"]
    attributeName :: T.Text
    attributeName = toAttributeName' name'

toNamePair :: Name -> T.Text
toNamePair (Name f b) = [qq|('{toAttributeName f}', '{I.toSnakeCaseText b}')|]

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

compileFieldInitializers :: DS.DeclarationSet Field -> CodeGen [Code]
compileFieldInitializers fields' =
    forM (toList fields') compileFieldInitializer
  where
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
compileDocstringWithFields indentSpace decl fields' =
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
        | f@(Field n _ _) <- toList fields'
        ]

compileDocsComment :: Documented a => Code -> a -> Code
compileDocsComment indentSpace d =
    case compileDocs d of
        Nothing -> "\n"
        Just rst -> indent (indentSpace `T.append` "#: ") rst

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
compileUnionTag source parentname d@(Tag typename' fields' _) = do
    abc <- collectionsAbc
    typeExprCodes <- mapM (compileTypeExpression' source)
        [Just typeExpr | (Field _ typeExpr _) <- fieldList]
    let nameTypeTriples = L.sortBy (compare `on` thd3)
                                   (zip3 tagNames typeExprCodes optionFlags)
    arg <- parameterCompiler
    pyVer <- getPythonVersion
    validators <- sequence
        [ do
              v <- compileValidator' source typeExpr $ toAttributeName' fName
              return (fName, typeExprCode, v)
        | (typeExprCode, Field fName typeExpr _) <- zip typeExprCodes fieldList
        ]
    deserializers <- sequence
        [ do
              deserializer <- compileDeserializer' source typeExpr
                  [qq|value.get('{I.toSnakeCaseText bName}')|]
                  [qq|rv_{toAttributeName fName}|]
                  [qq|error_{toAttributeName fName}|]
              return (fieldName', typeExpr, deserializer)
        | Field fieldName'@(Name fName bName) typeExpr _ <- fieldList
        ]
    initializers <- compileFieldInitializers fields'
    return $ toStrict $ renderMarkup $ [compileText|
class #{className}(#{parentClass}):
#{compileDocstringWithFields "    " d fields'}
    __slots__ = (
%{ forall Field fName _ _ <- fieldList }
        '#{toAttributeName' fName}',
%{ endforall }
    )
    __nirum_type__ = 'union'
    __nirum_tag__ = #{parentClass}.Tag.#{toEnumMemberName typename'}

    @staticmethod
    def __nirum_tag_types__():
        return [
%{ forall (n, t, _) <- nameTypeTriples }
            ('#{n}', #{t}),
%{ endforall }
        ]

%{ case pyVer }
%{ of Python2 }
    def __init__(self, **kwargs):
%{ of Python3 }
    def __init__(
%{ if null nameTypeTriples }
        self
%{ else }
        self, *, #{compileParameters arg nameTypeTriples}
%{ endif }
    ) -> None:
%{ endcase }
        _type_repr = __import__('typing')._type_repr
        # typing module can be masked by field name of the same name, e.g.:
        #     union foo = bar ( text typing );
        # As Nirum identifier disallows to begin with dash/underscore,
        # we can avoid such name overwrapping by defining _type_repr,
        # an underscore-leaded alias of typing._type_repr and using it
        # in the below __init__() inner function.
        def __init__(#{compileParameters arg nameTypeTriples}):
%{ forall (fName, fType, (Validator fTypePred fValueValidators)) <- validators }
            if not (#{fTypePred}):
                raise TypeError(
                    '#{toAttributeName' fName} must be a value of ' +
                    _type_repr(#{fType}) + ', not ' +
                    repr(#{toAttributeName' fName})
                )
%{ forall ValueValidator fValuePredCode fValueErrorMsg <- fValueValidators }
            elif not (#{fValuePredCode}):
                raise ValueError(
                    'invalid #{toAttributeName' fName}: '
                    #{stringLiteral fValueErrorMsg}
                )
%{ endforall }
%{ endforall }
%{ forall initializer <- initializers }
            #{initializer}
%{ endforall }
            pass  # it's necessary when there are no parameters at all
%{ case pyVer }
%{ of Python2 }
        __init__(**kwargs)
%{ of Python3 }
        __init__(
%{ forall Field fName _ _ <- fieldList }
            #{toAttributeName' fName}=#{toAttributeName' fName},
%{ endforall }
        )
%{ endcase }

    def __nirum_serialize__(self):
        return {
            '_type': '#{behindParentTypename}',
            '_tag': '#{behindTagName}',
%{ forall Field fName@(Name _ fBehind) fType _ <- fieldList }
            '#{ I.toSnakeCaseText fBehind}':
#{compileSerializer' source fType $ T.append "self." $ toAttributeName' fName},
%{ endforall }
        }

    @classmethod
%{ case pyVer }
%{ of Python2 }
    def __nirum_deserialize__(cls, value, on_error=None):
%{ of Python3 }
    def __nirum_deserialize__(
        cls: type,
        value,
        on_error: typing.Optional[
            typing.Callable[[typing.Tuple[str, str]], None]
        ]=None
    ) -> typing.Optional['#{className}']:
%{ endcase }
        errors = set()
        if on_error is None:
            def on_error(err_field, err_msg):
                errors.add((err_field, err_msg))
        errored = [False]
        def handle_error(err_field, err_msg):
            errored[0] = True
            on_error(err_field, err_msg)
        if isinstance(value, #{abc}.Mapping):
            try:
                tag = value['_tag']
            except KeyError:
                handle_error('._tag', 'Expected to exist.')
            else:
                if tag == '#{toBehindSnakeCaseText typename'}':
%{ forall (Name fName bName, typeExpr, deserializer) <- deserializers }
                    error_#{toAttributeName fName} = lambda ef, em: \
                        handle_error('.#{I.toSnakeCaseText bName}' + ef, em)
%{ case typeExpr }
%{ of OptionModifier _ }
                    if '#{I.toSnakeCaseText bName}' not in value:
                        value['#{I.toSnakeCaseText bName}'] = None
#{indent "                    " deserializer}
%{ of _ }
                    if '#{I.toSnakeCaseText bName}' in value:
#{indent "                        " deserializer}
                    else:
                        error_#{toAttributeName fName}('', 'Expected to exist.')
%{ endcase }
%{ endforall }
                    pass  # No-op; just for convenience' sake of the compiler
                else:
                    handle_error(
                        '._tag',
                        'Expected to be a "#{toBehindSnakeCaseText typename'}".'
                    )
        else:
            handle_error('', 'Expected an object.')
        if errors:
            raise ValueError(
                '\n'.join(sorted('{0}: {1}'.format(*e) for e in errors))
            )
        if not errored[0]:
            return cls(
%{ forall (fName, _, _) <- deserializers }
                #{toAttributeName' fName}=rv_#{toAttributeName' fName},
%{ endforall }
            )

%{ case pyVer }
%{ of Python2 }
    def __eq__(self, other):
%{ of Python3 }
    def __eq__(self, other: '#{parentClass}') -> bool:
%{ endcase }
        return isinstance(other, #{className}) and all(
            getattr(self, attr) == getattr(other, attr)
            for attr in self.__slots__
        )

%{ case pyVer }
%{ of Python2 }
    def __ne__(self, other):
%{ of Python3 }
    def __ne__(self, other: '#{parentClass}') -> bool:
%{ endcase }
        return not self == other

%{ case pyVer }
%{ of Python2 }
    def __hash__(self):
%{ of Python3 }
    def __hash__(self) -> int:
%{ endcase }
        return hash((
%{ forall Field fName _ _ <- fieldList }
            self.#{toAttributeName' fName},
%{ endforall }
        ))

%{ case pyVer }
%{ of Python2 }
    def __repr__(self):
%{ of Python3 }
    def __repr__(self) -> bool:
%{ endcase }
        return ''.join([
            '#{sourceImportPath source}.#{parentClass}.#{className}(',
%{ forall (i, Field fName _ _) <- enumerate fieldList }
%{ if i > 0 }
            ', ',
%{ endif }
            '#{toAttributeName' fName}=',
            repr(self.#{toAttributeName' fName }),
%{ endforall }
            ')'
        ])


#{parentClass}.#{className} = #{className}
if hasattr(#{parentClass}, '__qualname__'):
    (#{className}).__qualname__ = '#{parentClass}.#{className}'
|]
  where
    optionFlags :: [Bool]
    optionFlags = [ case typeExpr of
                        OptionModifier _ -> True
                        _ -> False
                  | (Field _ typeExpr _) <- toList fields'
                  ]
    className :: T.Text
    className = toClassName' typename'
    behindParentTypename :: T.Text
    behindParentTypename = I.toSnakeCaseText $ N.behindName parentname
    tagNames :: [T.Text]
    tagNames = map (toAttributeName' . fieldName) fieldList
    behindTagName :: T.Text
    behindTagName = I.toSnakeCaseText $ N.behindName typename'
    fieldList :: [Field]
    fieldList = toList fields'
    parentClass :: T.Text
    parentClass = toClassName' parentname

compileTypeExpression' :: Source -> Maybe TypeExpression -> CodeGen Code
compileTypeExpression' Source { sourceModule = boundModule } =
    compileTypeExpression boundModule

compileSerializer' :: Source -> TypeExpression -> Code -> Code
compileSerializer' Source { sourceModule = boundModule } =
    compileSerializer boundModule

compileValidator' :: Source -> TypeExpression -> Code -> CodeGen Validator
compileValidator' Source { sourceModule = boundModule } =
    compileValidator boundModule

compileDeserializer' :: Source
                     -> TypeExpression
                     -> Code
                     -> Code
                     -> Code
                     -> CodeGen Markup
compileDeserializer' Source { sourceModule = boundModule } =
    compileDeserializer boundModule

compileTypeDeclaration :: Source -> TypeDeclaration -> CodeGen Code
compileTypeDeclaration _ TypeDeclaration { type' = PrimitiveType {} } =
    return ""  -- never used
compileTypeDeclaration src d@TypeDeclaration { typename = typename'
                                             , type' = Alias ctype
                                             } = do
    ctypeExpr <- compileTypeExpression' src (Just ctype)
    return $ toStrict $ renderMarkup $ [compileText|
%{ case compileDocs d }
%{ of Just rst }
#: #{rst}
%{ of Nothing }
%{ endcase }
#{toClassName' typename'} = #{ctypeExpr}
    |]
compileTypeDeclaration src d@TypeDeclaration { typename = typename'
                                             , type' = UnboxedType itype
                                             } = do
    let className = toClassName' typename'
    itypeExpr <- compileTypeExpression' src (Just itype)
    insertStandardImport "typing"
    pyVer <- getPythonVersion
    Validator typePred valueValidators' <- compileValidator' src itype "value"
    deserializer <- compileDeserializer' src itype "value" "rv" "handle_error"
    return $ toStrict $ renderMarkup $ [compileText|
class #{className}(object):
#{compileDocstring "    " d}

    __nirum_type__ = 'unboxed'

%{ case pyVer }
%{ of Python2 }
    def __init__(self, value):
%{ of Python3 }
    def __init__(self, value: '#{itypeExpr}') -> None:
%{ endcase }
        if not (#{typePred}):
            raise TypeError(
                'expected {0}, not {1!r}'.format(
                    typing._type_repr(#{itypeExpr}),
                    value
                )
            )
%{ forall ValueValidator predCode msg <- valueValidators' }
        if not (#{predCode}):
            raise ValueError(#{stringLiteral msg})
%{ endforall }
        self.value = value  # type: #{itypeExpr}

%{ case pyVer }
%{ of Python2 }
    def __ne__(self, other):
        return not self == other

    def __eq__(self, other):
%{ of Python3 }
    def __eq__(self, other) -> bool:
%{ endcase }
        return (isinstance(other, #{className}) and
                self.value == other.value)

%{ case pyVer }
%{ of Python2 }
    def __hash__(self):
%{ of Python3 }
    def __hash__(self) -> int:
%{ endcase }
        return hash(self.value)

    def __nirum_serialize__(self):
        return (#{compileSerializer' src itype "self.value"})

    @classmethod
%{ case pyVer }
%{ of Python2 }
    def __nirum_deserialize__(cls, value, on_error=None):
%{ of Python3 }
    def __nirum_deserialize__(
        cls: type,
        value: typing.Any,
        on_error: typing.Optional[
            typing.Callable[[typing.Tuple[str, str]], None]
        ]=None
    ) -> typing.Optional['#{className}']:
%{ endcase }
        errors = set()
        if on_error is None:
            def on_error(err_field, err_msg):
                errors.add((err_field, err_msg))
        errored = [False]
        def handle_error(err_field, err_msg):
            errored[0] = True
            on_error(err_field, err_msg)
#{indent "        " deserializer}
        if errors:
            raise ValueError(
                '\n'.join(sorted('{0}: {1}'.format(*e) for e in errors))
            )
        if not errored[0]:
            return cls(rv)

%{ case pyVer }
%{ of Python2 }
    def __repr__(self):
        return '{0.__module__}.{0.__name__}({1!r})'.format(
            type(self), self.value
        )
%{ of Python3 }
    def __repr__(self) -> str:
        return '{0}({1!r})'.format(typing._type_repr(type(self)), self.value)
%{ endcase }

%{ case pyVer }
%{ of Python2 }
    def __hash__(self):
%{ of Python3 }
    def __hash__(self) -> int:
%{ endcase }
        return hash(self.value)
|]
compileTypeDeclaration _ d@TypeDeclaration { typename = typename'
                                           , type' = EnumType members'
                                           } = do
    let className = toClassName' typename'
    insertStandardImport "enum"
    insertStandardImport "typing"
    baseString <- baseStringClass
    pyVer <- getPythonVersion
    return $ toStrict $ renderMarkup [compileText|
class #{className}(enum.Enum):
#{compileDocstring "    " d}

%{ forall member@(EnumMember memberName@(Name _ behind) _) <- toList members' }
#{compileDocsComment "    " member}
    #{toEnumMemberName memberName} = '#{I.toSnakeCaseText behind}'
%{ endforall }

%{ case pyVer }
%{ of Python2 }
    def __nirum_serialize__(self):
%{ of Python3 }
    def __nirum_serialize__(self) -> str:
%{ endcase }
        return self.value

    @classmethod
%{ case pyVer }
%{ of Python2 }
    def __nirum_deserialize__(cls, value, on_error=None):
%{ of Python3 }
    def __nirum_deserialize__(
        cls: type,
        value: str,
        on_error: typing.Optional[
            typing.Callable[[typing.Tuple[str, str]], None]
        ]=None
    ) -> '#{className}':
%{ endcase }
        errors = set()
        if on_error is None:
            def on_error(err_field, err_msg):
                errors.add((err_field, err_msg))
        if isinstance(value, #{baseString}):
            member = value.replace('-', '_')
            try:
                result = cls(member)
            except ValueError:
                on_error(
                    '',
                    'Expected a string of a member name, but the given '
                    'string is not a member.  Available member names are: '
                    + ', '.join('"{0}"'.format(m.value) for m in cls)
                )
        else:
            on_error(
                '',
                'Expected a string of a member name, but the given value '
                'is not a string.'
            )
        if errors:
            raise ValueError(
                '\n'.join(sorted('{0}: {1}'.format(*e) for e in errors))
            )
        return result


# Since enum.Enum doesn't allow to define non-member when the class is defined,
# __nirum_type__ should be defined after the class is defined.
#{className}.__nirum_type__ = 'enum'
|]
compileTypeDeclaration src d@TypeDeclaration { typename = Name tnFacial tnBehind
                                             , type' = RecordType fields'
                                             } = do
    typeExprCodes <- mapM (compileTypeExpression' src)
        [Just typeExpr | (Field _ typeExpr _) <- fieldList]
    let nameTypeTriples = L.sortBy
            (compare `on` thd3)
            (zip3 [toAttributeName' name' | Field name' _ _ <- fieldList]
                  typeExprCodes optionFlags)
    insertStandardImport "typing"
    abc <- collectionsAbc
    arg <- parameterCompiler
    pyVer <- getPythonVersion
    validators <- sequence
        [ do
              v <- compileValidator' src typeExpr $ toAttributeName' fName
              return (fName, typeExprCode, v)
        | (typeExprCode, Field fName typeExpr _) <- zip typeExprCodes fieldList
        ]
    deserializers <- sequence
        [ do
              deserializer <- compileDeserializer' src typeExpr
                  [qq|value.get('{I.toSnakeCaseText bName}')|]
                  [qq|rv_{toAttributeName fName}|]
                  [qq|error_{toAttributeName fName}|]
              return (fieldName', typeExpr, deserializer)
        | Field fieldName'@(Name fName bName) typeExpr _ <- fieldList
        ]
    initializers <- compileFieldInitializers fields'
    return $ toStrict $ renderMarkup $ [compileText|
class #{className}(object):
#{compileDocstringWithFields "    " d fields'}
    __slots__ = (
%{ forall Field fName _ _ <- fieldList }
        '#{toAttributeName' fName}',
%{ endforall }
    )
    __nirum_type__ = 'record'

%{ case pyVer }
%{ of Python2 }
    def __init__(self, **kwargs):
%{ of Python3 }
    def __init__(
%{ if null nameTypeTriples }
        self
%{ else }
        self, *, #{compileParameters arg nameTypeTriples}
%{ endif }
    ) -> None:
%{ endcase }
        _type_repr = __import__('typing')._type_repr
        # typing module can be masked by field name of the same name, e.g.:
        #     record foo ( text typing );
        # As Nirum identifier disallows to begin with dash/underscore,
        # we can avoid such name overwrapping by defining _type_repr,
        # an underscore-leaded alias of typing._type_repr and using it
        # in the below __init__() inner function.
        def __init__(#{compileParameters arg nameTypeTriples}):
%{ forall (fName, fType, (Validator fTypePred fValueValidators)) <- validators }
            if not (#{fTypePred}):
                raise TypeError(
                    '#{toAttributeName' fName} must be a value of ' +
                    _type_repr(#{fType}) + ', not ' +
                    repr(#{toAttributeName' fName})
                )
%{ forall ValueValidator fValuePredCode fValueErrorMsg <- fValueValidators }
            elif not (#{fValuePredCode}):
                raise ValueError(
                    'invalid #{toAttributeName' fName}: '
                    #{stringLiteral fValueErrorMsg}
                )
%{ endforall }
%{ endforall }
%{ forall initializer <- initializers }
            #{initializer}
%{ endforall }
            pass  # it's necessary when there are no parameters at all
%{ case pyVer }
%{ of Python2 }
        __init__(**kwargs)
%{ of Python3 }
        __init__(
%{ forall Field fName _ _ <- fieldList }
            #{toAttributeName' fName}=#{toAttributeName' fName},
%{ endforall }
        )
%{ endcase }

%{ case pyVer }
%{ of Python2 }
    def __repr__(self):
%{ of Python3 }
    def __repr__(self) -> bool:
%{ endcase }
        return ''.join([
            '#{sourceImportPath src}.#{className}(',
%{ forall (i, Field fName _ _) <- enumerate fieldList }
%{ if i > 0 }
            ', ',
%{ endif }
            '#{toAttributeName' fName}=',
            repr(self.#{toAttributeName' fName }),
%{ endforall }
            ')'
        ])

%{ case pyVer }
%{ of Python2 }
    def __eq__(self, other):
%{ of Python3 }
    def __eq__(self, other: '#{className}') -> bool:
%{ endcase }
        return isinstance(other, #{className}) and all(
            getattr(self, attr) == getattr(other, attr)
            for attr in self.__slots__
        )

%{ case pyVer }
%{ of Python2 }
    def __ne__(self, other):
%{ of Python3 }
    def __ne__(self, other: '#{className}') -> bool:
%{ endcase }
        return not self == other

    def __nirum_serialize__(self):
        return {
            '_type': '#{I.toSnakeCaseText tnBehind}',
%{ forall Field fName@(Name _ fBehind) fType _ <- fieldList }
            '#{ I.toSnakeCaseText fBehind}':
#{compileSerializer' src fType $ T.append "self." $ toAttributeName' fName},
%{ endforall }
        }

    @classmethod
%{ case pyVer }
%{ of Python2 }
    def __nirum_deserialize__(cls, value, on_error=None):
%{ of Python3 }
    def __nirum_deserialize__(
        cls: type,
        value,
        on_error: typing.Optional[
            typing.Callable[[typing.Tuple[str, str]], None]
        ]=None
    ) -> typing.Optional['#{className}']:
%{ endcase }
        errors = set()
        if on_error is None:
            def on_error(err_field, err_msg):
                errors.add((err_field, err_msg))
        errored = [False]
        def handle_error(err_field, err_msg):
            errored[0] = True
            on_error(err_field, err_msg)
        if isinstance(value, #{abc}.Mapping):
%{ forall (Name fName bName, typeExpr, deserializer) <- deserializers }
            error_#{toAttributeName fName} = lambda ef, em: \
                handle_error('.#{I.toSnakeCaseText bName}' + ef, em)
%{ case typeExpr }
%{ of OptionModifier _ }
            if '#{I.toSnakeCaseText bName}' not in value:
                value['#{I.toSnakeCaseText bName}'] = None
#{indent "            " deserializer}
%{ of _ }
            if '#{I.toSnakeCaseText bName}' in value:
#{indent "                " deserializer}
            else:
                error_#{toAttributeName fName}('', 'Expected to exist.')
%{ endcase }
%{ endforall }
        else:
            handle_error('', 'Expected an object.')
        if errors:
            raise ValueError(
                '\n'.join(sorted('{0}: {1}'.format(*e) for e in errors))
            )
        if not errored[0]:
            return cls(
%{ forall (fName, _, _) <- deserializers }
                #{toAttributeName' fName}=rv_#{toAttributeName' fName},
%{ endforall }
            )

%{ case pyVer }
%{ of Python2 }
    def __hash__(self):
%{ of Python3 }
    def __hash__(self) -> int:
%{ endcase }
        return hash((
%{ forall Field fName _ _ <- fieldList }
            self.#{toAttributeName' fName},
%{ endforall }
        ))
|]
  where
    className = toClassName tnFacial
    fieldList :: [Field]
    fieldList = toList fields'
    optionFlags :: [Bool]
    optionFlags = [ case typeExpr of
                        OptionModifier _ -> True
                        _ -> False
                  | (Field _ typeExpr _) <- fieldList
                  ]
compileTypeDeclaration src
                       d@TypeDeclaration { typename = typename'
                                         , type' = union@UnionType {}
                                         , typeAnnotations = annotations
                                         } = do
    tagCodes <- mapM (compileUnionTag src typename') tags'
    abc <- collectionsAbc
    insertStandardImport "typing"
    insertStandardImport "enum"
    insertThirdPartyImportsA [("nirum.datastructures", [("map_type", "Map")])]
    baseString <- baseStringClass
    pyVer <- getPythonVersion
    return $ toStrict $ renderMarkup $ [compileText|
class #{className}(#{T.intercalate "," $ compileExtendClasses annotations}):
#{compileDocstring "    " d}

    __nirum_type__ = 'union'

    class Tag(enum.Enum):
%{ forall (Tag tn _ _) <- tags' }
        #{toEnumMemberName tn} = '#{toBehindSnakeCaseText tn}'
%{ endforall }

%{ case pyVer }
%{ of Python2 }
    def __init__(self, *args, **kwargs):
%{ of Python3 }
    def __init__(self, *args, **kwargs) -> None:
%{ endcase }
        raise NotImplementedError(
            "{0} cannot be instantiated "
            "since it is an abstract class.  Instantiate a concrete subtype "
            "of it instead.".format(typing._type_repr(self))
        )

%{ case pyVer }
%{ of Python2 }
    def __nirum_serialize__(self):
%{ of Python3 }
    def __nirum_serialize__(self) -> typing.Mapping[str, object]:
%{ endcase }
        raise NotImplementedError(
            "{0} cannot be instantiated "
            "since it is an abstract class.  Instantiate a concrete subtype "
            "of it instead.".format(typing._type_repr(self))
        )

    @classmethod
%{ case pyVer }
%{ of Python2 }
    def __nirum_deserialize__(cls, value, on_error=None):
%{ of Python3 }
    def __nirum_deserialize__(
        cls: type,
        value,
        on_error: typing.Optional[
            typing.Callable[[typing.Tuple[str, str]], None]
        ]=None
    ) -> '#{className}':
%{ endcase }
        errors = set()
        if on_error is None:
            def on_error(err_field, err_msg):
                errors.add((err_field, err_msg))
        errored = [False]
        def handle_error(err_field, err_msg):
            errored[0] = True
            on_error(err_field, err_msg)
        if isinstance(value, #{abc}.Mapping):
            try:
                tag = value['_tag']
            except KeyError:
%{ case defaultTag union }
%{ of Just dt }
                tag = '#{toBehindSnakeCaseText $ tagName dt}'
                value = dict(value)
                value['_tag'] = tag
%{ of Nothing }
                handle_error('._tag', 'Expected to exist.')
%{ endcase }
        else:
            handle_error('', 'Expected an object.')
        if errored[0]:
            pass
%{ forall (Tag tn _ _) <- tags' }
        elif tag == '#{toBehindSnakeCaseText tn}':
            rv = #{toClassName' tn}.__nirum_deserialize__(
                value, handle_error
            )
%{ endforall }
        elif isinstance(tag, #{baseString}):
            handle_error(
                '._tag',
                'Expected one of the following strings: '
%{ forall (i, (Tag tn _ _)) <- enumerate tags' }
%{ if i < 1 }
%{ elseif i < pred (length tags') }
                ', '
%{ else }
                ', or '
%{ endif }
                '"#{toBehindSnakeCaseText tn}"'
%{ endforall }
                '.'
            )
        else:
            handle_error(
                '._tag',
                'Expected a string, but the given value is not a string.'
            )
        if errors:
            raise ValueError(
                '\n'.join(sorted('{0}: {1}'.format(*e) for e in errors))
            )
        if not errored[0]:
            return rv

%{ forall tagCode <- tagCodes }
#{tagCode}

%{ endforall }

#{className}.__nirum_tag_classes__ = map_type({
%{ forall (Tag tn _ _) <- tags' }
    #{className}.Tag.#{toEnumMemberName tn}: #{toClassName' tn},
%{ endforall }
})
|]
  where
    tags' :: [Tag]
    tags' = DS.toList $ tags union
    className :: T.Text
    className = toClassName' typename'
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
                            ]
    insertThirdPartyImportsA
        [ ("nirum.constructs", [("name_dict_type", "NameDict")])
        , ("nirum.datastructures", [(nirumMapName, "Map")])
        , ("nirum.service", [("service_type", "Service")])
        , ("nirum.transport", [("transport_type", "Transport")])
        ]
    return [qq|
class $className(service_type):
{compileDocstring "    " d}
    __nirum_type__ = 'service'
    __nirum_schema_version__ = \'{SV.toText $ version metadata'}\'
    __nirum_service_methods__ = \{
        {methodMetadata'}
    \}
    __nirum_method_names__ = name_dict_type([
        $methodNameMap
    ])
    __nirum_method_annotations__ = $methodAnnotations'

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

{className}.Client = {className}_Client
{className}.Client.__name__ = 'Client'
if hasattr({className}.Client, '__qualname__'):
    {className}.Client.__qualname__ = '{className}.Client'
|]
  where
    nirumMapName :: T.Text
    nirumMapName = "map_type"
    className :: T.Text
    className = toClassName' name'
    commaNl :: [T.Text] -> T.Text
    commaNl = T.intercalate ",\n"
    compileErrorType :: Method -> CodeGen (Maybe Code)
    compileErrorType (Method mn _ _ me _) =
        case me of
            Just errorTypeExpression -> do
                et <- compileTypeExpression' src (Just errorTypeExpression)
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
        rtypeExpr <- compileTypeExpression' src rtype
        ret <- returnCompiler
        return [qq|
    def {mName'}(self, {commaNl params'}){ ret rtypeExpr }:
{compileDocstring' "        " m paramDocs}
        raise NotImplementedError('$className has to implement {mName'}()')
|]
    compileMethodParameter :: Parameter -> CodeGen Code
    compileMethodParameter (Parameter pName pType _) = do
        pTypeExpr <- compileTypeExpression' src (Just pType)
        arg <- parameterCompiler
        return [qq|{arg (toAttributeName' pName) pTypeExpr}|]
    compileMethodMetadata :: Method -> CodeGen Code
    compileMethodMetadata Method { methodName = mName
                                 , parameters = params
                                 , returnType = rtype
                                 } = do
        let params' = toList params :: [Parameter]
        rtypeExpr <- compileTypeExpression' src rtype
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
    methodList :: [Method]
    methodList = toList methods
    compileParameterMetadata :: Parameter -> CodeGen Code
    compileParameterMetadata (Parameter pName pType _) = do
        let pName' = toAttributeName' pName
        pTypeExpr <- compileTypeExpression' src (Just pType)
        return [qq|'{pName'}': lambda: $pTypeExpr|]
    methodNameMap :: T.Text
    methodNameMap = toIndentedCodes
        toNamePair
        [mName | Method { methodName = mName } <- methodList]
        ",\n        "
    paramNameMap :: [Parameter] -> T.Text
    paramNameMap params = toIndentedCodes
        toNamePair [pName | Parameter pName _ _ <- params] ",\n        "
    compileClientPayload :: Parameter -> CodeGen Code
    compileClientPayload (Parameter pName pt _) = do
        let pName' = toAttributeName' pName
        return [qq|'{I.toSnakeCaseText $ N.behindName pName}':
                   ({compileSerializer' src pt pName'})|]
    compileClientMethod :: Method -> CodeGen Code
    compileClientMethod Method { methodName = mName
                               , parameters = params
                               , returnType = rtypeM
                               , errorType = etypeM
                               } = do
        let clientMethodName' = toAttributeName' mName
        params' <- mapM compileMethodParameter $ toList params
        rtypeExpr <- compileTypeExpression' src rtypeM
        resultDeserializer <- case rtypeM of
            Just rtype -> compileDeserializer' src rtype
                "serialized"
                "result"
                "on_deserializer_error"
            Nothing ->
                return "result = None"
        errorDeserializer <- case etypeM of
             Just e -> compileDeserializer' src e
                "serialized"
                "result"
                "on_deserializer_error"
             Nothing -> do
                insertThirdPartyImportsA
                    [ ("nirum.exc", [ ("_unexpected_nirum_response_error"
                                      , "UnexpectedNirumResponseError"
                                      )
                                    ]
                      )
                    ]
                return "raise _unexpected_nirum_response_error(serialized)"
        payloadArguments <- mapM compileClientPayload $ toList params
        validators <- sequence
            [ do
                  v <- compileValidator' src pTypeExpr $ toAttributeName' pName
                  pTypeExprCode <- compileTypeExpression' src $ Just pTypeExpr
                  return (pName, pTypeExprCode, v)
            | Parameter pName pTypeExpr _ <- toList params
            ]
        ret <- returnCompiler
        return $ toStrict $ renderMarkup $ [compileText|
    def #{clientMethodName'}(self, #{commaNl params'})#{ret rtypeExpr}:
        _type_repr = __import__('typing')._type_repr
        # typing module can be masked by parameter of the same name, e.g.:
        #     service foo-service ( bar (text typing) );
        # As Nirum identifier disallows to begin with dash/underscore,
        # we can avoid such name overwrapping by defining _type_repr,
        # an underscore-leaded alias of typing._type_repr and using it
        # in the below.
%{ forall (pName, pType, (Validator pTypePred pValueValidators)) <- validators }
        if not (#{pTypePred}):
            raise TypeError(
                '#{toAttributeName' pName} must be a value of ' +
                _type_repr(#{pType}) + ', not ' +
                repr(#{toAttributeName' pName})
            )
%{ forall ValueValidator pValuePredCode pValueErrorMsg <- pValueValidators }
        elif not (#{pValuePredCode}):
            raise ValueError(
                'invalid #{toAttributeName' pName}: '
                #{stringLiteral pValueErrorMsg}
            )
%{ endforall }
%{ endforall }
        successful, serialized = self.__nirum_transport__(
            '#{I.toSnakeCaseText $ N.behindName mName}',
            payload={#{commaNl payloadArguments}},
            # FIXME Give annotations.
            service_annotations={},
            method_annotations=self.__nirum_method_annotations__,
            parameter_annotations={}
        )
        deserializer_errors = set()
        def on_deserializer_error(err_field, err_msg):
            deserializer_errors.add((err_field, err_msg))
        if successful:
#{indent "            " resultDeserializer}
        else:
#{indent "            " errorDeserializer}
        if deserializer_errors:
            raise ValueError(
                '\n'.join(
                    sorted('{0}: {1}'.format(*e) for e in deserializer_errors)
                )
            )
        if successful:
            return result
        raise result
|]
    toKeyItem :: I.Identifier -> T.Text -> T.Text
    toKeyItem ident v = [qq|'{toAttributeName ident}': {v}|]
    wrapMap :: T.Text -> T.Text
    wrapMap items = [qq|$nirumMapName(\{$items\})|]
    compileAnnotation :: I.Identifier -> A.AnnotationArgumentSet -> T.Text
    compileAnnotation ident annoArgument =
        toKeyItem ident $
            wrapMap $ T.intercalate ","
                [ [qq|'{toAttributeName ident'}': {annoArgToText value}|]
                | (ident', value) <- M.toList annoArgument
                ]
      where
        escapeSingle :: T.Text -> T.Text
        escapeSingle = T.strip . T.replace "'" "\\'"
        annoArgToText :: AnnotationArgument -> T.Text
        annoArgToText (AI.Text t) = [qq|u'''{escapeSingle t}'''|]
        annoArgToText (Integer i) = T.pack $ show i
    compileMethodAnnotation :: Method -> T.Text
    compileMethodAnnotation Method { methodName = mName
                                   , methodAnnotations = annoSet
                                   } =
        toKeyItem (N.facialName mName) $ wrapMap annotationDict
      where
        annotationDict :: T.Text
        annotationDict = T.intercalate ","
            [ compileAnnotation ident annoArgSet :: T.Text
            | (ident, annoArgSet) <- M.toList $ A.annotations annoSet
            ]
    methodAnnotations' :: T.Text
    methodAnnotations' = wrapMap $ commaNl $ map compileMethodAnnotation
        methodList

compileTypeDeclaration _ Import {} =
    return ""  -- Nothing to compile

compileModuleBody :: Source -> CodeGen Code
compileModuleBody src@Source { sourceModule = boundModule } = do
    let types' = boundTypes boundModule
    typeCodes <- mapM (compileTypeDeclaration src) $ toList types'
    return $ T.intercalate "\n\n" typeCodes

compileModule :: PythonVersion
              -> Source
              -> Either CompileError' ( S.Set T.Text
                                      , M.Map (Int, Int) (S.Set T.Text)
                                      , Code
                                      )
compileModule pythonVersion' source = do
    let (result, context) = runCodeGen (compileModuleBody source)
                                       (empty pythonVersion')
    let deps = S.union
            (dependencies context)
            (require "nirum" "nirum" $ M.keysSet $ thirdPartyImports context)
    let standardImportSet' = standardImportSet context
    let optDeps = M.unionWith S.union
            (optionalDependencies context)
            [ ((3, 4), require "enum34" "enum" standardImportSet')
            , ((3, 5), require "typing" "typing" standardImportSet')
            ]
    let fromImports = M.assocs (localImportsMap context) ++
                      M.assocs (thirdPartyImports context)
    code <- result
    return $ (deps, optDeps,) $ toStrict $ renderMarkup $
        [compileText|# -*- coding: utf-8 -*-
#{compileDocstring "" $ sourceModule source}
%{ forall (alias, import') <- M.assocs (standardImports context) }
%{ if import' == alias }
import #{import'}
%{ else }
import #{import'} as #{alias}
%{ endif }
%{ endforall }

%{ forall (from, nameMap) <- fromImports }
from #{from} import (
%{ forall (alias, name) <- M.assocs nameMap }
%{ if (alias == name) }
    #{name},
%{ else }
    #{name} as #{alias},
%{ endif }
%{ endforall }
)
%{ endforall }

#{code}
|]
  where
    has :: S.Set T.Text -> T.Text -> Bool
    has set module' = module' `S.member` set ||
                      any (T.isPrefixOf $ module' `T.snoc` '.') set
    require :: T.Text -> T.Text -> S.Set T.Text -> S.Set T.Text
    require pkg module' set =
        if set `has` module' then S.singleton pkg else S.empty

compilePackageMetadata :: Package'
                       -> (S.Set T.Text, M.Map (Int, Int) (S.Set T.Text))
                       -> Code
compilePackageMetadata Package
                           { metadata = MD.Metadata
                                 { authors = authors'
                                 , version = version'
                                 , description = description'
                                 , license = license'
                                 , MD.keywords = keywords'
                                 , target = target'@Python
                                       { packageName = packageName'
                                       , minimumRuntimeVersion = minRuntimeVer
                                       , classifiers = classifiers'
                                       }
                                 }
                           , modules = modules'
                           }
                       (deps, optDeps) =
    toStrict $ renderMarkup [compileText|# -*- coding: utf-8 -*-
import sys

from setuptools import setup, __version__ as setuptools_version

install_requires = [
%{ forall p <- S.toList deps }
%{ if (p == "nirum") }
    'nirum >= #{SV.toText minRuntimeVer}',
%{ else }
    (#{stringLiteral p}),
%{ endif }
%{ endforall }
]
polyfill_requires = {
%{ forall ((major, minor), deps') <- M.toList optDeps }
    (#{major}, #{minor}): [
%{ forall dep' <- S.toList deps' }
        (#{stringLiteral dep'}),
%{ endforall }
    ],
%{ endforall }
}

%{ if (not $ M.null optDeps) }
# '<' operator for environment markers are supported since setuptools 17.1.
# Read PEP 496 for details of environment markers.
setup_requires = ['setuptools >= 17.1']
if tuple(map(int, setuptools_version.split('.'))) < (17, 1):
    extras_require = {}
    if 'bdist_wheel' not in sys.argv:
        for (major, minor), deps in polyfill_requires.items():
            if sys.version_info < (major, minor):
                install_requires.extend(deps)
    envmarker = ":python_version=='{0}.{1}'"
    python_versions = [(2, 6), (2, 7),
                       (3, 3), (3, 4), (3, 5), (3, 6)]  # FIXME
    for pyver in python_versions:
        extras_require[envmarker.format(*pyver)] = list({
            d
            for v, vdeps in polyfill_requires.items()
            if pyver < v
            for d in vdeps
        })
else:
    extras_require = {
        ":python_version<'{0}.{1}'".format(*pyver): deps
        for pyver, deps in polyfill_requires.items()
    }
%{ else }
setup_requires = []
extras_require = {}
%{ endif }


SOURCE_ROOT = #{stringLiteral $ sourceDirectory Python3}

if sys.version_info < (3, 0):
    SOURCE_ROOT = #{stringLiteral $ sourceDirectory Python2}

# TODO: long_description, url
setup(
    name=#{stringLiteral packageName'},
    version=#{stringLiteral $ SV.toText version'},
    description=#{nStringLiteral description'},
    license=#{nStringLiteral license'},
    keywords=#{stringLiteral $ T.intercalate " " keywords'},
    classifiers=[
%{ forall classifier <- classifiers' }
    #{stringLiteral classifier},
%{ endforall }
    ],
    author=', '.join([
%{ forall Author { name = name } <- authors' }
    (#{stringLiteral name}),
%{ endforall }
    ]),
    author_email=', '.join([
%{ forall Author { email = email' } <- authors' }
%{ case email' }
%{ of Just authorEmail }
    (#{stringLiteral $ decodeUtf8 $ E.toByteString authorEmail}),
%{ of Nothing }
%{ endcase }
%{ endforall }
    ]),
    package_dir={'': SOURCE_ROOT},
    packages=[#{strings $ toImportPaths target' $ MS.keysSet modules'}],
    provides=[#{strings $ toImportPaths target' $ MS.keysSet modules'}],
    requires=[#{strings $ S.toList deps}],
    setup_requires=setup_requires,
    install_requires=install_requires,
    extras_require=extras_require,
    entry_points={
        'nirum.modules': [
%{ forall modPath <- MS.keys modules' }
            '#{normalizeModulePath modPath} = #{toImportPath target' modPath}',
%{ endforall }
        ],
        'nirum.classes': [
%{ forall (modPath, Module types' _) <- MS.toList modules' }
%{ forall typeName <- catMaybes (typeNames types') }
            '#{normalizeModulePath modPath}.#{I.toNormalizedText typeName} = '
            '#{toImportPath target' modPath}:#{toClassName typeName}',
%{ endforall }
%{ endforall }
        ],
    },
)
|]
  where
    normalizeModulePath :: ModulePath -> T.Text
    normalizeModulePath = T.intercalate "." . map I.toNormalizedText . toList
    typeNames :: DS.DeclarationSet TD.TypeDeclaration -> [Maybe I.Identifier]
    typeNames types' =
        [ case td of
              TD.TypeDeclaration { TD.typename = (Name n _) } -> Just n
              TD.ServiceDeclaration { TD.serviceName = (Name n _) } -> Just n
              TD.Import {} -> Nothing
        | td <- DS.toList types'
        ]
    nStringLiteral :: Maybe T.Text -> T.Text
    nStringLiteral (Just value) = stringLiteral value
    nStringLiteral Nothing = "None"
    strings :: [Code] -> Code
    strings values = T.intercalate ", " $ map stringLiteral (L.sort values)

manifestIn :: Code
manifestIn = [q|recursive-include src *.py
recursive-include src-py2 *.py
|]

compilePackage' :: Package'
                -> M.Map FilePath (Either CompileError' Code)
compilePackage' package@Package { metadata = MD.Metadata { target = target' }
                                } =
    M.fromList $
        initFiles ++
        [ ( f
          , case cd of
                Left e -> Left e
                Right (_, _, cd') -> Right cd'
          )
        | (f, cd) <- modules'
        ] ++
        [ ("setup.py", Right $ compilePackageMetadata package allDependencies)
        , ("MANIFEST.in", Right manifestIn)
        ]
  where
    toPythonFilename :: ModulePath -> [FilePath]
    toPythonFilename mp = [ T.unpack (toAttributeName i)
                          | i <- toList $ renameModulePath' target' mp
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
    modules' :: [ ( FilePath
                  , Either CompileError' ( S.Set T.Text
                                         , M.Map (Int, Int) (S.Set T.Text)
                                         , Code
                                         )
                  )
                ]
    modules' =
        [ ( toFilename (sourceDirectory ver) modulePath'
          , compileModule ver $ Source package boundModule
          )
        | (modulePath', _) <- MS.toAscList (modules package)
        , Just boundModule <- [resolveBoundModule modulePath' package]
        , ver <- versions
        ]
    allDependencies :: (S.Set T.Text, M.Map (Int, Int) (S.Set T.Text))
    allDependencies =
        ( S.unions [deps | (_, Right (deps, _, _)) <- modules']
        , M.unionsWith S.union [oDeps | (_, Right (_, oDeps, _)) <- modules']
        )

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
                                             MD.fieldType v
            | (k, v) <- HM.toList renameTable
            ]
        classifiers' <- case textArrayField "classifiers" table of
            Right t -> Right t
            Left (FieldError _) -> Right []
            otherwise' -> otherwise'
        return Python { packageName = name'
                      , minimumRuntimeVersion = max minRuntime minimumRuntime
                      , renames = M.fromList renamePairs
                      , classifiers = classifiers'
                      }
    compilePackage = compilePackage'
    showCompileError _ e = e
    toByteString _ = encodeUtf8
