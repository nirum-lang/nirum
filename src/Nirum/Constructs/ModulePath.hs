{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Nirum.Constructs.ModulePath ( ModulePath( ModuleName
                                               , ModulePath
                                               , moduleName
                                               , path
                                               )
                                   ) where

import GHC.Exts (IsList(Item, fromList, toList))

import Data.Text (intercalate)

import Nirum.Constructs (Construct(toCode))
import Nirum.Constructs.Identifier (Identifier)

data ModulePath = ModulePath { path :: ModulePath
                             , moduleName :: Identifier }
                | ModuleName { moduleName :: Identifier }
                deriving (Eq, Ord, Show)

instance Construct ModulePath where
    toCode = intercalate "." . map toCode . toList

instance IsList ModulePath where
    type Item ModulePath = Identifier
    fromList [] = error "ModulePath cannot be empty"
    fromList [identifier] = ModuleName identifier
    fromList identifiers =
        ModulePath (fromList $ init identifiers) $ last identifiers
    toList (ModuleName identifier) = [identifier]
    toList (ModulePath path' identifier) = toList path' ++ [identifier]
