{-# LANGUAGE OverloadedStrings #-}
module Nirum.Constructs.Name ( Name(Name)
                             , behindName
                             , facialName
                             , isComplex
                             , isSimple
                             , toCode
                             ) where

import Data.String (IsString(fromString))

import Data.Text (append, snoc)

import Nirum.Constructs (Construct)
import Nirum.Constructs.Identifier (Identifier, toCode)

-- | Name consists of a 'facialName' and a 'behindName'.
-- The 'behindName' is for internal coding, while the 'facialName' is for
-- human-readable identifier of programming language bindings.
--
-- If 'behind_Name' is omitted, 'facialName' is used also for
-- 'behindName'.  Explicit 'behindName' is useful to maintain
-- old name for backward compatibility with new 'facialName'.
data Name = Name { facialName :: Identifier
                 , behindName :: Identifier
                 } deriving (Eq, Ord, Show)

isSimple :: Name -> Bool
isSimple (Name f b) = f == b

isComplex :: Name -> Bool
isComplex = not . isSimple

instance Construct Name where
    toCode name@(Name facial behind)
        | isSimple name = toCode facial
        | otherwise = (toCode facial `snoc` '/') `append` toCode behind

instance IsString Name where
    fromString s =
        Name { facialName = identifier, behindName = identifier }
      where
        identifier :: Identifier
        identifier = fromString s
