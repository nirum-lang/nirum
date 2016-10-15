{-# LANGUAGE QuasiQuotes #-}
module Nirum.Constructs.Annotation.Internal ( Annotation ( Annotation
                                                         , metadata
                                                         , name
                                                         )
                                            , AnnotationSet ( AnnotationSet
                                                            , annotations
                                                            )
                                            , Metadata
                                            , fromTuple
                                            ) where

import qualified Data.Char as C
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Docs (annotationDocsName)
import Nirum.Constructs.Identifier (Identifier)


type Metadata = T.Text

-- | Annotation for 'Declaration'.
data Annotation = Annotation { name :: Identifier
                             , metadata :: Maybe Metadata
                             } deriving (Eq, Ord, Show)

instance Construct Annotation where
    toCode Annotation {name = n, metadata = Just m} =
        [qq|@{toCode n}("$m'")|]
      where
        m' = (showLitString $ T.unpack m) ""
        showLitString :: String -> ShowS
        showLitString = foldr ((.) . showLitChar') id
        showLitChar' :: Char -> ShowS
        showLitChar' '"' = showString "\\\""
        showLitChar' c = C.showLitChar c
    toCode Annotation {name = n, metadata = Nothing} = [qq|@{toCode n}|]

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
