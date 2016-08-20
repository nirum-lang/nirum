{-# LANGUAGE OverloadedStrings #-}
module Nirum.Constructs.Annotation ( Annotation(Annotation)
                                   , AnnotationSet
                                   , Metadata
                                   , NameDuplication(AnnotationNameDuplication)
                                   , annotations
                                   , docs
                                   , empty
                                   , fromList
                                   , insertDocs
                                   , lookup
                                   , lookupDocs
                                   , singleton
                                   , toCode
                                   , toList
                                   , union
                                   ) where

import Prelude hiding (lookup)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Annotation.Internal
import Nirum.Constructs.Declaration (Docs (Docs), annotationDocsName, toText)
import Nirum.Constructs.Identifier (Identifier)


docs :: Docs -> Annotation
docs (Docs d) = Annotation { name = annotationDocsName, metadata = Just d }

data NameDuplication = AnnotationNameDuplication Identifier
                     deriving (Eq, Ord, Show)

empty :: AnnotationSet
empty = AnnotationSet { annotations = M.empty }

singleton :: Annotation -> AnnotationSet
singleton Annotation { name = name', metadata = metadata' } =
    AnnotationSet { annotations = M.singleton name' metadata' }

fromList :: [Annotation] -> Either NameDuplication AnnotationSet
fromList annotations' =
    case findDup names S.empty of
        Just duplication -> Left (AnnotationNameDuplication duplication)
        _ -> Right $ AnnotationSet ( M.fromList [ (name a, metadata a)
                                                | a <- annotations'
                                                ]
                                   )
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
toList AnnotationSet { annotations = annotations' } =
    map fromTuple $ M.assocs annotations'

union :: AnnotationSet -> AnnotationSet -> AnnotationSet
union (AnnotationSet a) (AnnotationSet b) = AnnotationSet $ M.union a b

lookup :: Identifier -> AnnotationSet -> Maybe Annotation
lookup id' (AnnotationSet anno) = do
    metadata' <- M.lookup id' anno
    return Annotation { name = id', metadata = metadata' }

lookupDocs :: AnnotationSet -> Maybe Docs
lookupDocs annotationSet = do
    Annotation _ m <- lookup annotationDocsName annotationSet
    data' <- m
    return $ Docs data'

insertDocs :: (Monad m) => Docs -> AnnotationSet -> m AnnotationSet
insertDocs docs' (AnnotationSet anno) =
    case insertLookup annotationDocsName (Just $ toText docs') anno of
        (Just _ , _    ) -> fail "<duplicated>"
        (Nothing, anno') -> return $ AnnotationSet anno'
  where
    insertLookup :: Ord k => k -> a -> M.Map k a -> (Maybe a, M.Map k a)
    insertLookup = M.insertLookupWithKey (\_ a _ -> a)
