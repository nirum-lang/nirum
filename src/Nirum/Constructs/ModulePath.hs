{-# LANGUAGE TypeFamilies #-}
module Nirum.Constructs.ModulePath ( ModulePath ( ModuleName
                                                , ModulePath
                                                , moduleName
                                                , path
                                                )
                                   , fromFilePath
                                   , fromIdentifiers
                                   , hierarchy
                                   , hierarchies
                                   ) where

import Data.Char (toLower)
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Exts (IsList (Item, fromList, toList))

import qualified Data.Set as S
import Data.Text (intercalate, pack)
import System.FilePath (splitDirectories, stripExtension)

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Identifier (Identifier, fromText)

data ModulePath = ModulePath { path :: ModulePath
                             , moduleName :: Identifier
                             }
                | ModuleName { moduleName :: Identifier }
                deriving (Eq, Show)

instance Ord ModulePath where
    a <= b = toList a <= toList b

instance Construct ModulePath where
    toCode = intercalate "." . map toCode . toList

fromIdentifiers :: [Identifier] -> Maybe ModulePath
fromIdentifiers [] = Nothing
fromIdentifiers [identifier] = Just $ ModuleName identifier
fromIdentifiers identifiers = fmap (`ModulePath` last identifiers) init'
  where
    init' :: Maybe ModulePath
    init' = fromIdentifiers $ init identifiers

fromFilePath :: FilePath -> Maybe ModulePath
fromFilePath filePath =
    if length fileIdentifiers == length paths
    then fromIdentifiers fileIdentifiers
    else Nothing
  where
    replaceLast :: (a -> a) -> [a] -> [a]
    replaceLast _ [] = []
    replaceLast f l = init l ++ [f $ last l]
    paths :: [FilePath]
    paths = replaceLast (fromMaybe "" . stripExtension "nrm" . map toLower)
                        (splitDirectories filePath)
    fileIdentifiers :: [Identifier]
    fileIdentifiers = mapMaybe (fromText . pack) paths

hierarchy :: ModulePath -> S.Set ModulePath
hierarchy m@ModuleName {} = S.singleton m
hierarchy m@(ModulePath parent _) = m `S.insert` hierarchy parent

instance IsList ModulePath where
    type Item ModulePath = Identifier
    fromList identifiers =
        fromMaybe (error "ModulePath cannot be empty")
                  (fromIdentifiers identifiers)
    toList (ModuleName identifier) = [identifier]
    toList (ModulePath path' identifier) = toList path' ++ [identifier]

hierarchies :: S.Set ModulePath -> S.Set ModulePath
hierarchies modulePaths = S.unions $ toList $ S.map hierarchy modulePaths
