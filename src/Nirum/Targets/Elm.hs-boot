{-# OPTIONS_GHC -Wno-missing-methods -Wno-orphans #-}
module Nirum.Targets.Elm () where

import Nirum.Package.Metadata (Target (..))
import Nirum.Targets.Elm.CodeGen

instance Target Elm where
