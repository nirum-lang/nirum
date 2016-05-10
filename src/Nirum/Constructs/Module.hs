{-# LANGUAGE OverloadedStrings #-}
module Nirum.Constructs.Module (Module(..)) where

import qualified Data.Text as T

import Nirum.Constructs (Construct(toCode))
import Nirum.Constructs.Declaration (Docs)
import Nirum.Constructs.DeclarationSet (DeclarationSet, toList)
import Nirum.Constructs.TypeDeclaration (TypeDeclaration)

data Module = Module { types :: DeclarationSet TypeDeclaration
                     , docs :: Maybe Docs
                     } deriving (Eq, Ord, Show)

instance Construct Module where
    toCode (Module types' docs') =
        T.concat [ maybe "" ((`T.snoc` '\n') . toCode) docs'
                 , "\n"
                 , T.intercalate "\n\n" $ map toCode $ toList types'
                 , "\n"
                 ]
