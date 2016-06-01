{-# LANGUAGE OverloadedLists, TypeFamilies #-}
module Nirum.Constructs.DeclarationSet ( DeclarationSet()
                                       , NameDuplication( BehindNameDuplication
                                                        , FacialNameDuplication
                                                        )
                                       , empty
                                       , fromList
                                       , lookup
                                       , lookup'
                                       , null
                                       , null'
                                       , size
                                       , toList
                                       , union
                                       , (!)
                                       ) where

import Data.Maybe (fromJust)
import qualified GHC.Exts as L
import Prelude hiding (lookup, null)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Nirum.Constructs.Declaration (Declaration(name))
import Nirum.Constructs.Identifier (Identifier)
import Nirum.Constructs.Name (Name(Name, behindName, facialName))

data Declaration a => DeclarationSet a
    -- | The set of 'Declaration' values.
    -- Every 'name' has to be unique in the set.
    -- The order of declarations is maintained.
    = DeclarationSet { declarations :: M.Map Identifier a
                     , index :: ![Identifier]
                     }
    deriving (Eq, Ord, Show)

data NameDuplication = FacialNameDuplication !Name
                     | BehindNameDuplication !Name
                     deriving (Eq, Show)

empty :: Declaration a => DeclarationSet a
empty = DeclarationSet { declarations = M.empty
                       , index = []
                       }

fromList :: Declaration a => [a] -> Either NameDuplication (DeclarationSet a)
fromList declarations' =
    case findDup names facialName S.empty of
        Just dup -> Left $ FacialNameDuplication dup
        _ -> case findDup names behindName S.empty of
            Just dup -> Left $ BehindNameDuplication dup
            _ -> Right DeclarationSet { declarations = M.fromList mapList
                                      , index = index'
                                      }
  where
    names :: [Name]
    names = map name declarations'
    index' :: [Identifier]
    index' = map facialName names
    mapList = [(facialName (name d), d) | d <- declarations']
    findDup :: [Name] -> (Name -> Identifier) -> S.Set Identifier -> Maybe Name
    findDup names' f dups =
        case names' of
            x:xs -> let name' = f x
                    in if name' `S.member` dups
                       then Just x
                       else findDup xs f $ S.insert name' dups
            _ -> Nothing

toList :: Declaration a => DeclarationSet a -> [a]
toList (DeclarationSet declarations' index') =
    map ((M.!) declarations') index'

size :: Declaration a => DeclarationSet a -> Int
size (DeclarationSet declarations' _) = M.size declarations'

null :: Declaration a => DeclarationSet a -> Bool
null = null'

null' :: Declaration a => DeclarationSet a -> Bool
null' (DeclarationSet declarations' _) = M.null declarations'

lookup :: Declaration a => Identifier -> DeclarationSet a -> Maybe a
lookup = lookup'

lookup' :: Declaration a => Identifier -> DeclarationSet a -> Maybe a
lookup' facialName' (DeclarationSet declarations' _) =
    M.lookup facialName' declarations'

(!) :: Declaration a => DeclarationSet a -> Identifier -> a
declarationSet ! facialName' = fromJust $ lookup' facialName' declarationSet

union :: Declaration a
      => DeclarationSet a
      -> DeclarationSet a
      -> Either NameDuplication (DeclarationSet a)
union a b = fromList $ toList a ++ toList b

instance (Declaration a) => L.IsList (DeclarationSet a) where
    type Item (DeclarationSet a) = a
    fromList declarations' =
        case fromList declarations' of
            Right set -> set
            Left (FacialNameDuplication (Name fname _)) ->
                error ("identifiers must be unique; " ++ show fname ++
                       " is defined more than once")
            Left (BehindNameDuplication (Name _ bname)) ->
                error ("behind names must be unique; " ++ show bname ++
                        " is defined more than once")
    toList = toList
