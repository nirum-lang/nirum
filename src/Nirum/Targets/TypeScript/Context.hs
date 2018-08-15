module Nirum.Targets.TypeScript.Context
    ( Context ( Context
              , localImports
              , thirdPartyImports
              )
    , CodeBuilder
    , TypeScript ( TypeScript, packageName )
    , empty
    ) where

import Data.Set hiding (empty)
import Data.Map.Strict hiding (empty)
import qualified Data.Map.Strict as MS
import Data.Text hiding (empty)

import qualified Nirum.CodeBuilder as CB
import Nirum.Constructs.ModulePath

newtype TypeScript = TypeScript { packageName :: Text }
                   deriving (Eq, Ord, Show)

data Context =
    Context { localImports :: Map ModulePath (Set Text)
            , thirdPartyImports :: Map ModulePath (Map Text Text)
            }
    deriving (Eq, Show)

type CodeBuilder t = CB.CodeBuilder t Context

empty :: Context
empty = Context { localImports = MS.empty
                , thirdPartyImports = MS.empty
                }
