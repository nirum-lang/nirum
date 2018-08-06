{-# LANGUAGE ExtendedDefaultRules, OverloadedLists, TypeSynonymInstances #-}
module Nirum.Targets.Rust.Keyword ( isPossibleKeyword
                                  , toRustIdentifier
                                  ) where

import qualified Data.Set as S
import qualified Data.Text as T

import qualified Nirum.Constructs.Identifier as I

-- | The set of Rust keywords.
-- See also: https://doc.rust-lang.org/reference/keywords.html
strictKeywords :: S.Set T.Text
strictKeywords =
    [ "as", "box", "break", "const", "continue"
    , "crate", "else", "enum", "extern", "false"
    , "fn", "for", "if", "impl", "in", "let"
    , "loop", "match", "mod", "move", "mut", "pub"
    , "ref", "return", "self", "Self", "static"
    , "struct", "super", "trait", "true", "type"
    , "unsafe", "use", "where", "while"
    ]
weakKeywords :: S.Set T.Text
weakKeywords =
    [ "catch", "default", "union", "'static" ]
reservedKeywords :: S.Set T.Text
reservedKeywords =
    [ "abstract", "alignof", "become", "do"
    , "final", "macro", "offsetof", "override"
    , "priv", "proc", "pure", "sizeof", "typeof"
    , "unsized", "virtual", "yield"
    ]

isPossibleKeyword :: T.Text -> Bool
isPossibleKeyword name' =
    (findMember strictKeywords) ||
    (findMember weakKeywords) ||
    (findMember reservedKeywords)
  where
    findMember :: S.Set T.Text -> Bool
    findMember = S.member name'

toRustIdentifier :: (I.Identifier -> T.Text) -> I.Identifier -> T.Text
toRustIdentifier convertIdent identifier =
    if isPossibleKeyword attrName then attrName `T.snoc` '_' else attrName
  where
    attrName :: T.Text
    attrName = convertIdent identifier
