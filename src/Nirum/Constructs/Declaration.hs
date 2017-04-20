{-# LANGUAGE DefaultSignatures #-}
module Nirum.Constructs.Declaration ( Declaration (annotations, name)
                                    , Documented (docs, docsBlock)
                                    ) where

import Nirum.Constructs (Construct)
import Nirum.Constructs.Annotation (AnnotationSet, lookupDocs)
import Nirum.Constructs.Docs (Docs, toBlock)
import Nirum.Constructs.Name (Name)
import Nirum.Docs (Block)

class Documented a where
    -- | The docs of the construct.
    docs :: a -> Maybe Docs
    default docs :: Declaration a => a -> Maybe Docs
    docs = lookupDocs . annotations

    -- | The parsed docs tree.
    docsBlock :: a -> Maybe Block
    docsBlock = fmap toBlock . docs

-- Construct which has its own unique 'name' and can has its 'docs'.
class (Construct a, Documented a) => Declaration a where
    name :: a -> Name
    annotations :: a -> AnnotationSet
