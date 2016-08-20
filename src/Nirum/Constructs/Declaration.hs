module Nirum.Constructs.Declaration ( Declaration
                                    , docs
                                    , name
                                    ) where

import Nirum.Constructs (Construct)
-- import Nirum.Constructs.Annotation (AnnotationSet)
import Nirum.Constructs.Docs (Docs)
import Nirum.Constructs.Name (Name)

-- 'Construct' which has its own unique 'name' and can has its 'docs'.
class Construct a => Declaration a where
    name        :: a -> Name
    docs        :: a -> Maybe Docs
