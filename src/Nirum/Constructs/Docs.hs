{-# LANGUAGE OverloadedStrings #-}
module Nirum.Constructs.Docs ( Docs (Docs)
                             , annotationDocsName
                             , toCode
                             , toCodeWithPrefix
                             , toText
                             ) where

import Data.String (IsString (fromString))

import qualified Data.Text as T

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Identifier (Identifier)

annotationDocsName :: Identifier
annotationDocsName = "docs"

-- | Docstring for constructs.
newtype Docs = Docs T.Text deriving (Eq, Ord, Show)

-- | Convert the docs to text.
toText :: Docs -> T.Text
toText (Docs docs') = docs'

-- | Similar to 'toCode' except it takes 'Maybe Docs' instead of 'Docs'.
-- If the given docs is 'Nothing' it simply returns an empty string.
-- Otherwise it returns a code string with the prefix.
toCodeWithPrefix :: T.Text     -- ^ The prefix to be prepended if not empty.
                 -> Maybe Docs -- ^ The docs to convert to code.
                 -> T.Text
toCodeWithPrefix _ Nothing = ""
toCodeWithPrefix prefix (Just docs') = T.append prefix $ toCode docs'

instance Construct Docs where
    toCode (Docs docs') = let d = if "\n" `T.isSuffixOf` docs'
                                  then T.dropEnd 1 docs'
                                  else docs'
                          in T.append "# " $ T.replace "\n" "\n# " d

instance IsString Docs where
    fromString s = let t = T.pack s
                   in Docs (if "\n" `T.isSuffixOf` t
                            then t
                            else t `T.snoc` '\n')
