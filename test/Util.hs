module Util (singleDocs) where

import Nirum.Constructs.Annotation as A (AnnotationSet, singleton, docs)
import Nirum.Constructs.Docs (Docs)

singleDocs :: Docs -> AnnotationSet
singleDocs = A.singleton . A.docs
