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
                            , withLocalImport
                            , withPackage
                            , withStandardImport
                            , withThirdPartyImport
                            ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs.DeclarationSet (toList)
import Nirum.Constructs.Identifier (toText)
import Nirum.Constructs.Module (Module(Module, types))
import Nirum.Constructs.Name (Name(Name))
import qualified Nirum.Constructs.Name as N
import Nirum.Constructs.TypeDeclaration ( EnumMember(EnumMember)
                                        , Type(Alias, BoxedType, EnumType)
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

withThirdPartyImport :: T.Text -> T.Text -> CodeGen a -> CodeGen a
withThirdPartyImport module' object c@CodeGen { thirdPartyImports = ti } =
    c { thirdPartyImports = M.insertWith S.union module' [object] ti }

withLocalImport :: T.Text -> T.Text -> CodeGen a -> CodeGen a
withLocalImport module' object c@CodeGen { localImports = li } =
    c { localImports = M.insertWith S.union module' [object] li }

compileTypeExpression :: TypeExpression -> CodeGen Code
compileTypeExpression (TypeIdentifier i) = return $ toText i
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
compileTypeDeclaration (TypeDeclaration (Name typename _) (Alias ctype) _) = do
    ctypeExpr <- compileTypeExpression ctype
    return [qq|
        # TODO: docstring
        $typename = $ctypeExpr
    |]
compileTypeDeclaration (TypeDeclaration typename (BoxedType itype) _) = do
    let facialName' = toText $ N.facialName typename
    itypeExpr <- compileTypeExpression itype
    withStandardImport "typing" $
        withThirdPartyImport "nirum.validate" "validate_boxed_type" $
            withThirdPartyImport "nirum.serialize" "serialize_boxed_type" $
                withThirdPartyImport "nirum.deserialize" "deserialize_boxed_type" $
                    return [qq|
class $facialName':
    # TODO: docstring

    def __init__(self, value: $itypeExpr) -> None:
        validate_boxed_type(value, $itypeExpr)
        self.value = value  # type: $itypeExpr

    def __eq__(self, other) -> bool:
        return (isinstance(other, $facialName') and
                self.value == other.value)

    def __hash__(self) -> int:
        return hash(self.value)

    def __nirum_serialize__(self) -> typing.Mapping[str, typing.Any]:
        return serialize_boxed_type(self)

    @classmethod
    def __nirum_deserialize__(
        cls: type, value: typing.Mapping[str, typing.Any]
    ) -> '{facialName'}':
        return deserialize_boxed_type(cls, value)

    def __repr__(self) -> str:
        return '\{0.__module__\}.\{0.__qualname__\}(\{1!r\})'.format(
            type(self), self.value
        )
                |]
compileTypeDeclaration (TypeDeclaration typename (EnumType members) _) = do
    let Name facialName _ = typename
        facialName' = toText facialName
        memberNames = T.intercalate
            "\n    "
            [ [qq|{toText fn} = '{toText bn}'|]
            | EnumMember (Name fn bn) _ <- toList members
            ]
    withStandardImport "enum" $ return [qq|
class $facialName'(enum.Enum):
    # TODO: docstring

    $memberNames

    def __nirum_serialize__(self) -> str:
        return self.value

    @classmethod
    def __nirum_deserialize__(cls: type, value: str) -> '{facialName'}':
        return cls(value)  # FIXME: validate input
    |]

compileTypeDeclaration TypeDeclaration {} =
    return "# TODO"

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

float64 = float  # FIXME
text = str

{code code'}
    |]
  where
    code' :: CodeGen T.Text
    code' = compileModuleBody module'
    imports :: S.Set T.Text -> T.Text
    imports importSet =
        if S.null importSet
        then ""
        else "import " `T.append` T.intersperse ',' (S.elems importSet)
    fromImports :: M.Map T.Text (S.Set T.Text) -> T.Text
    fromImports importMap =
        T.intersperse '\n'
            [ [qq|from $from import {T.intercalate ", " $ S.elems vars}|]
            | (from, vars) <- M.assocs importMap
            ]
