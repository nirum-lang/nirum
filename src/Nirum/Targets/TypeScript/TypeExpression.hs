{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.TypeScript.TypeExpression (compileTypeExpression) where 

import Text.Blaze
import Text.Heterocephalus

import Nirum.Constructs.TypeExpression
import {-# SOURCE #-} Nirum.Targets.TypeScript ()
import Nirum.Targets.TypeScript.Context
import Nirum.TypeInstance.BoundModule


compileTypeExpression :: BoundModule TypeScript -> TypeExpression -> CodeBuilder TypeScript Markup
compileTypeExpression _ _ = return [compileText|any|]
