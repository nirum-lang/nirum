{-# LANGUAGE QuasiQuotes #-}
module Nirum.Constructs.Annotation.Internal
    ( Annotation ( Annotation
                 , arguments
                 , name
                 )
    , AnnotationArgument ( Text
                         , Int
                         )
    , AnnotationArgumentSet
    , AnnotationSet ( AnnotationSet
                    , annotations
                    )
    ) where

import qualified Data.Char as C

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Docs
import Nirum.Constructs.Identifier (Identifier)

data AnnotationArgument = Text T.Text
                        | Int Integer
                        deriving (Eq, Ord, Show)

type AnnotationArgumentSet = M.Map Identifier AnnotationArgument

-- | Annotation for 'Declaration'.
data Annotation = Annotation { name :: Identifier
                             , arguments :: AnnotationArgumentSet
                             } deriving (Eq, Ord, Show)

instance Construct Annotation where
    toCode Annotation { name = n, arguments = args }
      | M.null args = '@' `T.cons` toCode n
      | otherwise = [qq|@{toCode n}({showArgs $ M.toList args})|]
      where
        showArgs :: [(Identifier, AnnotationArgument)] -> T.Text
        showArgs args' = T.intercalate ", " $ map showArg args'
        showArg :: (Identifier, AnnotationArgument) -> T.Text
        showArg (key, value) = [qq|{toCode key} = {argToText value}|]
        argToText :: AnnotationArgument -> T.Text
        argToText (Text t) = literal t
        argToText (Int i) = T.pack $ show i
        literal :: T.Text -> T.Text
        literal s = [qq|"{(showLitString $ T.unpack s) ""}"|]
        showLitString :: String -> ShowS
        showLitString = foldr ((.) . showLitChar') id
        showLitChar' :: Char -> ShowS
        showLitChar' '"' = showString "\\\""
        showLitChar' c = C.showLitChar c

newtype AnnotationSet
  -- | The set of 'Annotation' values.
  -- Every annotation name has to be unique in the set.
  = AnnotationSet { annotations :: M.Map Identifier AnnotationArgumentSet }
  deriving (Eq, Ord, Show)

instance Construct AnnotationSet where
    toCode AnnotationSet {annotations = annotations'} =
        T.concat [ s
                 | (name', args) <- M.assocs annotations'
                 , name' /= docsAnnotationName
                 , s <- [toCode $ Annotation name' args, "\n"]
                 ]
