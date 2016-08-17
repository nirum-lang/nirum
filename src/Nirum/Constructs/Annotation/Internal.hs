{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Nirum.Constructs.Annotation.Internal ( Annotation(..)
                                            , AnnotationSet(..)
                                            , Metadata
                                            , fromTuple
                                            ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Declaration (annotationDocsName)
import Nirum.Constructs.Identifier (Identifier)


type Metadata = T.Text

-- | Annotation for 'Declaration'.
data Annotation = Annotation { name :: Identifier
                             , metadata :: Maybe Metadata
                             } deriving (Eq, Ord, Show)

instance Construct Annotation where
    toCode Annotation {name = n,  metadata = Just m} =
        let m' = T.replace "\"" "\\\"" m in [qq|@{toCode n}("$m'")|]
    toCode Annotation {name = n,  metadata = Nothing} = [qq|@{toCode n}|]

fromTuple :: (Identifier, Maybe Metadata) -> Annotation
fromTuple (name', meta') = Annotation { name = name', metadata = meta' }

data AnnotationSet
  -- | The set of 'Annotation' values.
  -- Every annotation name has to be unique in the set.
  = AnnotationSet { annotations :: M.Map Identifier (Maybe Metadata) }
  deriving (Eq, Ord, Show)

instance Construct AnnotationSet where
    toCode AnnotationSet {annotations = annotations'} =
        T.concat [s | e <- M.assocs annotations'
                    , fst e /= annotationDocsName
                    , s <- [toCode $ fromTuple e, "\n"]]
