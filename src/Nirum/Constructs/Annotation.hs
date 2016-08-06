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

import Text.InterpolatedString.Perl6 (qq)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Nirum.Constructs (Construct(toCode))
import qualified Nirum.Constructs.Identifier as CI


type Metadata = T.Text

-- | Annotation for names.
data Annotation = Annotation { name :: CI.Identifier
                             , metadata :: Metadata
                             } deriving (Eq, Ord, Show)

instance Construct Annotation where
    toCode Annotation {name = n,  metadata = m} = [qq|[{CI.toCode n}: "$m"]|]

data AnnotationSet
  -- | The set of 'Annotation' values.
  -- Evenry annotaiton name has to be unique in the set.
  = AnnotationSet { annotations :: M.Map CI.Identifier Annotation }
  deriving (Eq, Ord, Show)

data NameDuplication = AnnotationNameDuplication CI.Identifier
                     deriving (Eq, Show)

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
    names :: [CI.Identifier]
    names = [name a | a <- annotations']
    findDup :: [CI.Identifier] -> S.Set CI.Identifier -> Maybe CI.Identifier
    findDup identifiers dups =
        case identifiers of
            x:xs -> if x `S.member` dups
                    then Just x
                    else findDup xs $ S.insert x dups
            _ -> Nothing

toList :: AnnotationSet -> [Annotation]
toList AnnotationSet { annotations = annotations' } = M.elems annotations'
