{-# LANGUAGE OverloadedLists, TypeSynonymInstances #-}
module Nirum.Targets.Rust.ModuleTree ( RustModule ( RustModule
                                                  , filePath
                                                  , modPath
                                                  , children
                                                  )
                                     ) where

import qualified Data.Set as S

import Nirum.Constructs.Identifier
import Nirum.Constructs.ModulePath (ModulePath)

data RustModule = RustModule { filePath :: FilePath
                             , modPath :: ModulePath
                             , children :: S.Set Identifier
                             }
    deriving (Eq, Show)
