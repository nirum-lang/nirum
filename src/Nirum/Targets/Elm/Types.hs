{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.Elm.Types (compileType) where

import GHC.Exts (IsList (..))

import Text.Blaze (Markup)
import Text.Heterocephalus (compileText)

import Nirum.Constructs.Identifier
import Nirum.Constructs.Name
import Nirum.Constructs.TypeDeclaration
import {-# SOURCE #-} Nirum.Targets.Elm ()
import Nirum.Targets.Elm.CodeGen
import Nirum.Targets.Elm.TypeExpression
import Nirum.TypeInstance.BoundModule

compileType :: BoundModule Elm -> TypeDeclaration -> CodeGen Markup

compileType boundModule
            TypeDeclaration { type' = Alias canon
                            , typename = Name { facialName = fName}
                            } = do
    typeExpr <- compileTypeExpression boundModule canon
    return [compileText|
type alias #{toPascalCaseText fName} = #{typeExpr}
|]

compileType _ TypeDeclaration { type' = EnumType members'
                              , typename = Name { facialName = fName}
                              } =
    return [compileText|
type #{toPascalCaseText fName}
%{ forall (i, EnumMember memberName _) <- enumerate (toList members') }
%{ if i < 1 }
    = #{toPascalCaseText $ facialName memberName}
%{ else }
    | #{toPascalCaseText $ facialName memberName}
%{ endif }
%{ endforall }
|]

compileType boundModule
            TypeDeclaration { type' = UnboxedType innerType'
                            , typename = Name { facialName = fName}
                            } = do
    typeExpr <- compileTypeExpression boundModule innerType'
    return [compileText|
type #{toPascalCaseText fName}
    = #{toPascalCaseText fName} (#{typeExpr})
|]

compileType _ _ =
    return [compileText||]
