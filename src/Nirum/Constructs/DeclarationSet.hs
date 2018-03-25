{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Nirum.Constructs.DeclarationSet ( DeclarationSet ()
                                       , NameDuplication ( BehindNameDuplication
                                                         , FacialNameDuplication
                                                         )
                                       , delete
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

import qualified Data.List as List
import Data.Maybe (fromJust)
import qualified GHC.Exts as L
import Prelude hiding (lookup, null)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Nirum.Constructs.Declaration (Declaration (..))
import Nirum.Constructs.Identifier (Identifier)
import Nirum.Constructs.Name (Name (Name, behindName, facialName))

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

fromList :: forall a . Declaration a
         => [a]
         -> Either NameDuplication (DeclarationSet a)
fromList decls =
    case ( List.find (\ d -> facialName (name d) `S.member` extraPublicNames d)
                     decls
         , findDup decls publicNames S.empty
         , findDup decls (S.singleton . behindName . name) S.empty
         ) of
        (Nothing, Nothing, Nothing) ->
            Right DeclarationSet
                { declarations = M.fromList mapList
                , index = index'
                }
        (Just dup, _, _) ->
            Left $ FacialNameDuplication (name dup)
        (Nothing, Just dup, _) ->
            Left $ FacialNameDuplication dup
        (Nothing, Nothing, Just dup) ->
            Left $ BehindNameDuplication dup
  where
    names :: [Name]
    names = map name decls
    index' :: [Identifier]
    index' = map facialName names
    mapList = [(facialName (name d), d) | d <- decls]
    publicNames :: a -> S.Set Identifier
    publicNames decl = facialName (name decl) `S.insert` extraPublicNames decl
    findDup :: [a]
            -> (a -> S.Set Identifier)
            -> S.Set Identifier
            -> Maybe Name
    findDup decls' f dups =
        case decls' of
            x : xs ->
                let publicNames' = f x
                    xName = name x
                    shadowings = publicNames' `S.intersection` dups
                in
                    case S.lookupMin shadowings of
                        Nothing -> findDup xs f $ S.union publicNames' dups
                        Just shadowing ->
                            if facialName xName `S.member` shadowings ||
                               behindName xName `S.member` shadowings
                            then Just xName
                            else Just (Name shadowing shadowing)
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

delete :: Declaration a
       => a
       -> DeclarationSet a
       -> DeclarationSet a
delete d DeclarationSet { declarations = ds, index = ix } =
    DeclarationSet
        { declarations = M.delete identifier ds
        , index = List.delete identifier ix
        }
  where
    identifier :: Identifier
    identifier = facialName $ name d

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
