module Nirum.Constructs (Construct, toCode) where

import Data.Text (Text)

class Ord a => Construct a where
    toCode :: a -> Text
