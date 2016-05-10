{-# LANGUAGE ExtendedDefaultRules, OverloadedLists, OverloadedStrings,
             QuasiQuotes #-}
module Nirum.Targets.Python ( Code
                            , CodeGen( code
                                     , localImports
                                     , packages
                                     , standardImports
                                     , thirdPartyImports
                                     )
                            , compileTypeDeclaration
                            , compileTypeExpression
                            , withLocalImport
                            , withPackage
                            , withStandardImport
                            , withThirdPartyImport
                            ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs.Identifier (toText)
import Nirum.Constructs.Name (Name(Name))
import Nirum.Constructs.TypeDeclaration ( Type(Alias, BoxedType, EnumType)
                                        , TypeDeclaration(TypeDeclaration)
                                        )
import Nirum.Constructs.TypeExpression ( TypeExpression( ListModifier
                                                       , MapModifier
                                                       , OptionModifier
                                                       , SetModifier
                                                       , TypeIdentifier
                                                       )
                                       )

type Code = Text

data CodeGen a = CodeGen { packages :: S.Set Text
                         , standardImports :: S.Set Text
                         , thirdPartyImports :: M.Map Text (S.Set Text)
                         , localImports :: M.Map Text (S.Set Text)
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

withPackage :: Text -> CodeGen a -> CodeGen a
withPackage package c@CodeGen { packages = p } =
    c { packages = S.insert package p }

withStandardImport :: Text -> CodeGen a -> CodeGen a
withStandardImport module' c@CodeGen { standardImports = si } =
    c { standardImports = S.insert module' si }

withThirdPartyImport :: Text -> Text -> CodeGen a -> CodeGen a
withThirdPartyImport module' object c@CodeGen { thirdPartyImports = ti } =
    c { thirdPartyImports = M.insertWith S.union module' [object] ti }

withLocalImport :: Text -> Text -> CodeGen a -> CodeGen a
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
    className :: Text
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
    let Name facialName' _ = typename
    itypeExpr <- compileTypeExpression itype
    withThirdPartyImport "nirum" "serialize_boxed_type" $ return [qq|
        class $facialName':
            # TODO: docstring

            def __init__(self, value: $itypeExpr) -> None:
                self.value = value  # type: $itypeExpr

            def __eq__(self, other) -> bool:
                return (isinstance(other, $facialName') and
                        self.value == other.value)

            def _hash__(self) -> int:
                return hash(self.value)

            def __nirum_serialize__(self) -> typing.Mapping[str, typing.Any]:
                return serialize_boxed_type()

            @classmethod
            def __nirum_deserialize__(
                cls: type, value: typing.Mapping[str, typing.Any]
            ) -> '{facialName'}':
                return
    |]
compileTypeDeclaration (TypeDeclaration typename (EnumType _) _) = do
    let Name facialName _ = typename
    withStandardImport "enum" $ return [qq|
        class $facialName(enum.Enum):
            # TODO: docstring

            ...
    |]

compileTypeDeclaration TypeDeclaration {} =
    return "# TODO"
