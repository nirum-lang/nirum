module Nirum.Constructs (Construct, toCode) where

import Data.Text (Text)

class (Eq a, Ord a) => Construct a where
    toCode :: a -> Text
