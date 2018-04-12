{-# LANGUAGE RankNTypes, StandaloneDeriving #-}
module Nirum.TypeInstance.BoundModule
    ( BoundModule (boundPackage, modulePath)
    , TypeLookup (..)
    , boundTypes
    , findInBoundModule
    , lookupType
    , resolveBoundModule
    ) where

import Nirum.Constructs.Declaration
import Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Identifier
import Nirum.Constructs.Module
import Nirum.Constructs.ModulePath
import Nirum.Constructs.TypeDeclaration as TypeDeclaration hiding (modulePath)
import Nirum.Package
import Nirum.Package.Metadata
import qualified Nirum.Package.ModuleSet as ModuleSet

data BoundModule t = BoundModule
    { boundPackage :: Target t => Package t
    , modulePath :: ModulePath
    }

deriving instance (Eq t, Target t) => Eq (BoundModule t)
deriving instance (Ord t, Target t) => Ord (BoundModule t)
deriving instance (Show t, Target t) => Show (BoundModule t)

resolveBoundModule :: ModulePath -> Package t -> Maybe (BoundModule t)
resolveBoundModule path' package =
    case resolveModule path' package of
        Just _ -> Just $ BoundModule package path'
        Nothing -> Nothing

findInBoundModule :: Target t => (Module -> a) -> a -> BoundModule t -> a
findInBoundModule valueWhenExist valueWhenNotExist
                  BoundModule { boundPackage = Package { modules = ms }
                              , modulePath = path'
                              } =
    case ModuleSet.lookup path' ms of
        Nothing -> valueWhenNotExist
        Just mod' -> valueWhenExist mod'

boundTypes :: Target t => BoundModule t -> DeclarationSet TypeDeclaration
boundTypes = findInBoundModule types DS.empty

data TypeLookup = Missing
                | Local Type
                | Imported ModulePath Identifier Type
                deriving (Eq, Ord, Show)

lookupType :: Target t => Identifier -> BoundModule t -> TypeLookup
lookupType identifier boundModule =
    case DS.lookup identifier (boundTypes boundModule) of
        Nothing ->
            toType
                coreModulePath
                identifier
                (DS.lookup identifier $ types coreModule)
        Just TypeDeclaration { type' = t } -> Local t
        Just (Import path' _ s _) ->
            case resolveModule path' (boundPackage boundModule) of
                Nothing -> Missing
                Just (Module decls _) ->
                    toType path' s (DS.lookup s decls)
        Just ServiceDeclaration {} -> Missing
  where
    toType :: ModulePath -> Identifier -> Maybe TypeDeclaration -> TypeLookup
    toType mp i (Just TypeDeclaration { type' = t }) = Imported mp i t
    toType _ _ _ = Missing

instance Target t => Documented (BoundModule t) where
    docs = findInBoundModule Nirum.Constructs.Module.docs Nothing
