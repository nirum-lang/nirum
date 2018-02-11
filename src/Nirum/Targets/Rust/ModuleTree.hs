{-# LANGUAGE OverloadedLists, TypeSynonymInstances #-}
module Nirum.Targets.Rust.ModuleTree ( RustModule ( RustModule
                                                  , filePath
                                                  , modPath
                                                  , children
                                                  )
                                     , buildRustModuleList
                                     ) where

import Data.List
import qualified Data.Text as T
import Data.Tree

import GHC.Exts (IsList (toList))

import System.FilePath (joinPath)

import Nirum.Constructs.Identifier
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Targets.Rust.Keyword

data UnpackedModule = UnpackedModule { unpackedModulePath :: [Identifier]
                                     , originalModulePath :: ModulePath
                                     }
data ModuleNode = ModuleNode { moduleName :: Identifier
                             , moduleNodePath :: Maybe ModulePath
                             }

instance Eq UnpackedModule where
    a == b = (unpackedModulePath a) == (unpackedModulePath b)
instance Ord UnpackedModule where
    a <= b = (unpackedModulePath a) <= (unpackedModulePath b)

type ModuleTree = Tree ModuleNode

data RustModule = RustModule { filePath :: FilePath
                             , modPath :: Maybe ModulePath
                             , children :: [Identifier]
                             }

-- type a = Identifier
-- type b = (Identifier, [UnpackedModule])
-- moduleUnfolder :: b -> (a, [b])
moduleUnfolder :: (ModuleNode, [UnpackedModule])
               -> (ModuleNode, [(ModuleNode, [UnpackedModule])])
moduleUnfolder (ident, mps) =
    (ident, groupByParent mps)
  where
    isParentEqual :: UnpackedModule -> UnpackedModule -> Bool
    isParentEqual UnpackedModule { unpackedModulePath = (a:_) }
                  UnpackedModule { unpackedModulePath = (b:_) } =
        a == b
    isParentEqual _ _ = False
    extractCommonParent :: [UnpackedModule] -> (ModuleNode, [UnpackedModule])
    extractCommonParent mps' =
        ( ModuleNode { moduleName = commonParent
                     , moduleNodePath = maybeModulePath
                     }
        , [ UnpackedModule { unpackedModulePath = x:xs
                           , originalModulePath = omn
                           }
          | UnpackedModule { unpackedModulePath = x:xs
                           , originalModulePath = omn
                           } <- mps'
          ]
        )
      where
        commonParent :: Identifier
        commonParent = head $ unpackedModulePath $ head mps'
        maybeModulePath :: Maybe ModulePath
        maybeModulePath =
            fmap originalModulePath $
            find (((==) 1) . length . unpackedModulePath) mps'
    groupByParent :: [UnpackedModule] -> [(ModuleNode, [UnpackedModule])]
    groupByParent = (map extractCommonParent) . (groupBy isParentEqual) . sort

buildModuleTree :: [ModulePath] -> ModuleTree
buildModuleTree mps =
    unfoldTree moduleUnfolder seed
  where
    srcModule :: ModuleNode
    srcModule = ModuleNode { moduleName = "src"
                           , moduleNodePath = Nothing
                           }
    seed :: (ModuleNode, [UnpackedModule])
    seed = ( srcModule
           , [ UnpackedModule { unpackedModulePath = toList mp
                              , originalModulePath = mp
                              }
             | mp <- mps
             ]
           )

toRustModuleList :: [String] -> ModuleTree -> [RustModule]
toRustModuleList baseDir Node { rootLabel = ModuleNode { moduleName = modName
                                                       , moduleNodePath = modPath'
                                                       }
                              , subForest = children'
                              } =
    RustModule { filePath = joinPath identPath
               , modPath = modPath'
               , children = map (moduleName . rootLabel) children'
               } :
    (concat $ map (toRustModuleList identPath) children')
  where
    libOrMod :: String
    libOrMod = case baseDir of
        [] -> "lib.rs"
        _ -> "mod.rs"
    identPath :: [String]
    identPath =
        baseDir ++
        [T.unpack $ toRustIdentifier toSnakeCaseText modName] ++
        [libOrMod]

buildRustModuleList :: [ModulePath] -> [RustModule]
buildRustModuleList = (toRustModuleList []) . buildModuleTree
