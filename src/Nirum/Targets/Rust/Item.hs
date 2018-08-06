{-# LANGUAGE ExtendedDefaultRules, OverloadedLists, TypeSynonymInstances,
             QuasiQuotes #-}
module Nirum.Targets.Rust.Item ( renderItem
                               ) where

import qualified Data.Text.Lazy as TL

import GHC.Exts (IsList (toList))

import Text.Blaze.Renderer.Text
import Text.Heterocephalus (compileText)

import qualified Nirum.Constructs.Identifier as I
import Nirum.Constructs.Name
import Nirum.Constructs.TypeDeclaration
import Nirum.Targets.Rust.Keyword

renderItem :: TypeDeclaration -> TL.Text
renderItem TypeDeclaration { typename = moduleName
                           , type' = ty
                           } =
    renderType ty
  where
    renderType :: Type -> TL.Text
    renderType EnumType { members = members' } =
        renderMarkup [compileText|
pub enum #{ toRustIdentifier I.toPascalCaseText $ facialName moduleName } {
%{ forall EnumMember memberName _ <- toList members' }
    #{ toRustIdentifier I.toPascalCaseText $ facialName memberName },
%{ endforall }
}
|]
    renderType _ = TL.empty
renderItem _ = TL.empty
