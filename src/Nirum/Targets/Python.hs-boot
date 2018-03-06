{-# OPTIONS_GHC -Wno-missing-methods -Wno-orphans #-}
module Nirum.Targets.Python () where

import Nirum.Package.Metadata (Target (..))
import Nirum.Targets.Python.CodeGen as CG

instance Target Python where
