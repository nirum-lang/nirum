{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Nirum.Constructs.Annotation ( Annotation(Annotation)
                                   , Metadata
                                   , toCode
                                   ) where

import Text.InterpolatedString.Perl6 (qq)
import qualified Data.Text as T

import Nirum.Constructs (Construct(toCode))


type Metadata = T.Text

-- | Annotation for names.
data Annotation = Annotation { name :: T.Text
                             , metadata :: Metadata
                             } deriving (Eq, Ord, Show)

instance Construct Annotation where
    toCode Annotation {name = n,  metadata = m} = [qq|[$n: "$m"]|]
