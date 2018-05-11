module Nirum.Constructs.Annotation
    ( Annotation (Annotation, name, arguments)
    , AnnotationSet
    , AnnotationArgumentSet
    , NameDuplication (AnnotationNameDuplication)
    , annotations
    , docs
    , empty
    , fromList
    , insertDocs
    , lookup
    , lookupDocs
    , null
    , singleton
    , toCode
    , toList
    , union
    ) where

import Prelude hiding (lookup, null)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Annotation.Internal
import Nirum.Constructs.Docs
import Nirum.Constructs.Identifier (Identifier)

docs :: Docs -> Annotation
docs (Docs d) = Annotation { name = docsAnnotationName
                           , arguments =
                                 M.singleton docsAnnotationParameter $ AText d
                           }

newtype NameDuplication = AnnotationNameDuplication Identifier
                          deriving (Eq, Ord, Show)

empty :: AnnotationSet
empty = AnnotationSet { annotations = M.empty }

null :: AnnotationSet -> Bool
null (AnnotationSet m) = M.null m

singleton :: Annotation -> AnnotationSet
singleton Annotation { name = name', arguments = args } =
    AnnotationSet { annotations = M.singleton name' args }

fromList :: [Annotation] -> Either NameDuplication AnnotationSet
fromList annotations' =
    case findDup names S.empty of
        Just duplication -> Left (AnnotationNameDuplication duplication)
        _ -> Right . AnnotationSet . M.fromList $
            [(n, a) | Annotation n a <- annotations']
  where
    names :: [Identifier]
    names = [name a | a <- annotations']
    findDup :: [Identifier] -> S.Set Identifier -> Maybe Identifier
    findDup identifiers dups =
        case identifiers of
            x : xs -> if x `S.member` dups
                      then Just x
                      else findDup xs $ S.insert x dups
            _ -> Nothing

toList :: AnnotationSet -> [Annotation]
toList AnnotationSet { annotations = annotations' } =
    [Annotation n a | (n, a) <- M.assocs annotations']

union :: AnnotationSet -> AnnotationSet -> AnnotationSet
union (AnnotationSet a) (AnnotationSet b) = AnnotationSet $ M.union a b

lookup :: Identifier -> AnnotationSet -> Maybe Annotation
lookup id' (AnnotationSet anno) = do
    args <- M.lookup id' anno
    return $ Annotation id' args

lookupDocs :: AnnotationSet -> Maybe Docs
lookupDocs annotationSet = do
    Annotation _ args <- lookup docsAnnotationName annotationSet
    d <- M.lookup docsAnnotationParameter args
    case d of
        AText d' -> Just $ Docs d'
        _ -> Nothing

insertDocs :: (Monad m) => Docs -> AnnotationSet -> m AnnotationSet
insertDocs docs' (AnnotationSet anno) =
    case insertLookup docsAnnotationName args anno of
        (Just _ , _) -> fail "<duplicated>"
        (Nothing, anno') -> return $ AnnotationSet anno'
  where
    insertLookup :: Ord k => k -> a -> M.Map k a -> (Maybe a, M.Map k a)
    insertLookup = M.insertLookupWithKey $ \ _ a _ -> a
    args :: AnnotationArgumentSet
    args = M.singleton docsAnnotationParameter $ AText $ toText docs'
