module Nirum.Constructs.TypeExpression ( TypeExpression ( ListModifier
                                                        , MapModifier
                                                        , OptionModifier
                                                        , SetModifier
                                                        , TypeIdentifier
                                                        , elementType
                                                        , identifier
                                                        , keyType
                                                        , type'
                                                        , valueType
                                                        )
                                       , toCode
                                       ) where

import Data.String (IsString (fromString))

import qualified Data.Text as T

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Identifier (Identifier)

-- | Refers a type.
data TypeExpression
    = TypeIdentifier { identifier :: Identifier }
    | OptionModifier { type' :: TypeExpression }
    | SetModifier { elementType :: TypeExpression }
    | ListModifier { elementType :: TypeExpression }
    | MapModifier { keyType :: TypeExpression
                  , valueType :: TypeExpression
                  }
    deriving (Eq, Ord, Show)

instance Construct TypeExpression where
    toCode (TypeIdentifier id') = toCode id'
    toCode (OptionModifier type_) = toCode type_ `T.snoc` '?'
    toCode (SetModifier element) =
        '{' `T.cons` toCode element `T.snoc` '}'
    toCode (ListModifier element) =
        '[' `T.cons` toCode element `T.snoc` ']'
    toCode (MapModifier key value) =
        T.concat ["{", toCode key, ": ", toCode value, "}"]

instance IsString TypeExpression where
    fromString string = TypeIdentifier (fromString string :: Identifier)
