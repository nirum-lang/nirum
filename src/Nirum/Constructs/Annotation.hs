{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Nirum.Constructs.Annotation ( Annotation(Annotation)
                                   , AnnotationSet(AnnotationSet)
                                   , Metadata
                                   , NameDuplication(AnnotationNameDuplication)
                                   , annotations
                                   , empty
                                   , fromList
                                   , toCode
                                   , toList
                                   ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Identifier (Identifier)


type Metadata = T.Text

-- | Annotation for 'Declaration'.
data Annotation = Annotation { name :: Identifier
                             , metadata :: Metadata
                             } deriving (Eq, Ord, Show)

instance Construct Annotation where
    toCode Annotation {name = n,  metadata = m} = [qq|[{toCode n}: "$m"]|]

data AnnotationSet
  -- | The set of 'Annotation' values.
  -- Every annotation name has to be unique in the set.
  = AnnotationSet { annotations :: M.Map Identifier Annotation }
  deriving (Eq, Ord, Show)

instance Construct AnnotationSet where
    toCode AnnotationSet {annotations = annotations'} =
        T.concat [s | e <- M.elems annotations', s <- [toCode e, "\n"]]

data NameDuplication = AnnotationNameDuplication Identifier
                     deriving (Eq, Ord, Show)

empty :: AnnotationSet
empty = AnnotationSet { annotations = M.empty }

fromList :: [Annotation] -> Either NameDuplication AnnotationSet
fromList annotations' =
    case findDup names S.empty of
        Just duplication -> Left (AnnotationNameDuplication duplication)
        _ -> Right AnnotationSet { annotations = M.fromList [ (name a, a)
                                                            | a <- annotations'
                                                            ]
                                 }
  where
    names :: [Identifier]
    names = [name a | a <- annotations']
    findDup :: [Identifier] -> S.Set Identifier -> Maybe Identifier
    findDup identifiers dups =
        case identifiers of
            x:xs -> if x `S.member` dups
                    then Just x
                    else findDup xs $ S.insert x dups
            _ -> Nothing

toList :: AnnotationSet -> [Annotation]
toList AnnotationSet { annotations = annotations' } = M.elems annotations'
