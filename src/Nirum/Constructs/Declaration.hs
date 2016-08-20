module Nirum.Constructs.Declaration ( Declaration
                                    , annotations
                                    , docs
                                    , name
                                    ) where

import Nirum.Constructs (Construct)
import Nirum.Constructs.Annotation (AnnotationSet, lookupDocs)
import Nirum.Constructs.Docs (Docs)
import Nirum.Constructs.Name (Name)

-- 'Construct' which has its own unique 'name' and can has its 'docs'.
class Construct a => Declaration a where
    name        :: a -> Name
    annotations :: a -> AnnotationSet

docs :: Declaration a => a -> Maybe Docs
docs = lookupDocs . annotations
