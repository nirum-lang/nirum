{-# OPTIONS_GHC -Wno-missing-methods -Wno-orphans #-}
module Nirum.Targets.TypeScript () where

import Nirum.Package.Metadata (Target (..))
import Nirum.Targets.TypeScript.Context

instance Target TypeScript where
