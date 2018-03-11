module Nirum.Package.ModuleSet ( ImportError ( CircularImportError
                                             , MissingImportError
                                             , MissingModulePathError
                                             )
                               , ModuleSet
                               , fromList
                               , fromMap
                               , keys
                               , keysSet
                               , length
                               , lookup
                               , null
                               , toAscList
                               , toList
                               , toMap
                               ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Prelude hiding (length, lookup, null)

import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Identifier (Identifier)
import Nirum.Constructs.Module (Module (Module), imports)
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Constructs.TypeDeclaration ( ImportName (impName)
                                        , importScopeName
                                        , TypeDeclaration ( Import
                                                          , ServiceDeclaration
                                                          , TypeDeclaration
                                                          )
                                        )

data ImportError = CircularImportError [ModulePath]
                 | MissingModulePathError ModulePath ModulePath
                 | MissingImportError ModulePath ModulePath Identifier
                 deriving (Eq, Ord, Show)

-- | The set of 'Module' values.  It can be looked up by its 'ModulePath'.
newtype ModuleSet = ModuleSet (M.Map ModulePath Module) deriving (Eq, Ord, Show)

fromMap :: M.Map ModulePath Module -> Either (S.Set ImportError) ModuleSet
fromMap ms
    | S.null importErrors = Right moduleSet
    | otherwise = Left importErrors
  where
    moduleSet :: ModuleSet
    moduleSet = ModuleSet ms
    importErrors :: S.Set ImportError
    importErrors = detectImportErrors moduleSet

fromList :: [(ModulePath, Module)] -> Either (S.Set ImportError) ModuleSet
fromList = fromMap . M.fromList

toMap :: ModuleSet -> M.Map ModulePath Module
toMap (ModuleSet ms) = ms

toList :: ModuleSet -> [(ModulePath, Module)]
toList = M.toList . toMap

toAscList :: ModuleSet -> [(ModulePath, Module)]
toAscList = M.toAscList . toMap

length :: ModuleSet -> Int
length = M.size . toMap

null :: ModuleSet -> Bool
null = M.null . toMap

keys :: ModuleSet -> [ModulePath]
keys = M.keys . toMap

keysSet :: ModuleSet -> S.Set ModulePath
keysSet = M.keysSet . toMap

lookup :: ModulePath -> ModuleSet -> Maybe Module
lookup path = M.lookup path . toMap

detectImportErrors :: ModuleSet -> S.Set ImportError
detectImportErrors moduleSet = detectMissingImports moduleSet `S.union`
                               detectCircularImports moduleSet

detectMissingImports :: ModuleSet -> S.Set ImportError
detectMissingImports moduleSet =
    S.fromList [e | (path, mod') <- toList moduleSet, e <- detect path mod']
  where
    detect :: ModulePath -> Module -> [ImportError]
    detect path module' =
        [ e
        | (path', importNames') <- M.toList (imports module')
        , e <- case lookup path' moduleSet of
                Nothing -> [MissingModulePathError path path']
                Just (Module decls _) ->
                    [ e
                    | i <- S.toList importNames'
                    , e <- case DS.lookup (impName i) decls of
                        Just TypeDeclaration {} -> []
                        Just ServiceDeclaration {} -> []
                        Just Import {} -> [ MissingImportError
                                                path
                                                path'
                                                (importScopeName i)
                                          ]
                        Nothing -> [ MissingImportError
                                         path
                                         path'
                                         (importScopeName i)
                                   ]
                    ]
        ]

detectCircularImports :: ModuleSet -> S.Set ImportError
detectCircularImports (ModuleSet ms) =
    S.fromList [e | path <- M.keys ms, e <- detect path []]
  where
    moduleImports :: M.Map ModulePath (S.Set ModulePath)
    moduleImports =
        M.fromList [ (path, M.keysSet $ imports module')
                   | (path, module') <- M.toList ms
                   ]
    detect :: ModulePath -> [ModulePath] -> [ImportError]
    detect path reversedCycle
        | path `elem` reversedCycle =
            [CircularImportError $ reverse reversedCycle']
        | otherwise =
            case M.lookup path moduleImports of
                Just paths -> [ e
                              | path' <- S.toList paths
                              , e <- detect path' reversedCycle'
                              ]
                Nothing -> []
      where
        reversedCycle' :: [ModulePath]
        reversedCycle' = path : reversedCycle
