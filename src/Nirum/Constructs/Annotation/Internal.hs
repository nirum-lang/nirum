{-# LANGUAGE QuasiQuotes #-}
module Nirum.Constructs.Annotation.Internal
    ( Annotation ( Annotation
                 , arguments
                 , name
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

type AnnotationArgumentSet = M.Map Identifier T.Text

-- | Annotation for 'Declaration'.
data Annotation = Annotation { name :: Identifier
                             , arguments :: AnnotationArgumentSet
                             } deriving (Eq, Ord, Show)

instance Construct Annotation where
    toCode Annotation { name = n, arguments = args }
      | M.null args = '@' `T.cons` toCode n
      | otherwise = [qq|@{toCode n}({showArgs $ M.toList args})|]
      where
        showArgs :: [(Identifier, T.Text)] -> T.Text
        showArgs args' = T.intercalate ", " $ map showArg args'
        showArg :: (Identifier, T.Text) -> T.Text
        showArg (key, value) = [qq|{toCode key} = {literal value}|]
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
