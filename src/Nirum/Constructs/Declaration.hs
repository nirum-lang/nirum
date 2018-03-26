{-# LANGUAGE DefaultSignatures #-}
module Nirum.Constructs.Declaration ( Declaration (..)
                                    , Documented (docs, docsBlock)
                                    ) where

import Data.Set (Set, empty)

import Nirum.Constructs (Construct)
import Nirum.Constructs.Annotation (AnnotationSet, lookupDocs)
import Nirum.Constructs.Docs (Docs, toBlock)
import Nirum.Constructs.Identifier (Identifier)
import Nirum.Constructs.Name (Name (..))
import Nirum.Docs (Block)

class Documented a where
    -- | The docs of the construct.
    docs :: a -> Maybe Docs
    default docs :: Declaration a => a -> Maybe Docs
    docs = lookupDocs . annotations

    -- | The parsed docs tree.
    docsBlock :: a -> Maybe Block
    docsBlock = fmap toBlock . docs

-- | Construct which has its own unique 'name' and can has its 'docs'.
class (Construct a, Documented a) => Declaration a where
    name :: a -> Name
    annotations :: a -> AnnotationSet

    -- | A set of facial name identifiers that are public (or "exported")
    -- but not used for name resolution.  (If an identifier should be a key
    -- for resolution 'name' should return it.)
    --
    -- The default implementation simply returns an empty set.
    --
    -- Intended to be used for 'Nirum.Constructs.DeclarationSet.DeclarationSet'.
    extraPublicNames :: a -> Set Identifier
    extraPublicNames _ = empty
