{-# LANGUAGE OverloadedLists, OverloadedStrings, QuasiQuotes  #-}
module Nirum.Constructs.Module ( Module(Module, docs, types)
                               , coreModule
                               , coreModulePath
                               , coreTypes
                               , imports
                               ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (q)

import Nirum.Constructs (Construct(toCode))
import Nirum.Constructs.Annotation (empty)
import Nirum.Constructs.Declaration (Docs)
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Identifier (Identifier)
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Constructs.TypeDeclaration ( JsonType(..)
                                        , PrimitiveTypeIdentifier(..)
                                        , Type(..)
                                        , TypeDeclaration(..)
                                        )

data Module = Module { types :: DS.DeclarationSet TypeDeclaration
                     , docs :: Maybe Docs
                     } deriving (Eq, Ord, Show)

instance Construct Module where
    toCode m@(Module types' docs') =
        T.concat [ maybe "" ((`T.snoc` '\n') . toCode) docs'
                 , T.intercalate "\n" importCodes
                 , if null importCodes then "\n" else "\n\n"
                 , T.intercalate "\n\n" typeCodes
                 , "\n"
                 ]
      where
        typeList :: [TypeDeclaration]
        typeList = DS.toList types'
        importCodes :: [T.Text]
        importCodes =
            [ T.concat [ "import ", toCode p, " ("
                       , T.intercalate ", " $ map toCode $ S.toAscList i
                       , ");"
                       ]
            | (p, i) <- M.toAscList (imports m)
            ]
        typeCodes :: [T.Text]
        typeCodes = [ toCode t
                    | t <- typeList
                    , case t of
                          Import {} -> False
                          _ -> True
                    ]

imports :: Module -> M.Map ModulePath (S.Set Identifier)
imports (Module decls _) =
    M.fromListWith S.union [(p, [i]) | Import p i <- DS.toList decls]

coreModulePath :: ModulePath
coreModulePath = ["core"]

coreModule :: Module
coreModule = Module coreTypes $ Just coreDocs

coreTypes :: DS.DeclarationSet TypeDeclaration
coreTypes =
    -- number types
    [ TypeDeclaration "bigint" (PrimitiveType Bigint String) Nothing empty
    , TypeDeclaration "decimal" (PrimitiveType Decimal String) Nothing empty
    , TypeDeclaration "int32" (PrimitiveType Int32 Number) Nothing empty
    , TypeDeclaration "int64" (PrimitiveType Int64 Number) Nothing empty
    , TypeDeclaration "float32" (PrimitiveType Float32 Number) Nothing empty
    , TypeDeclaration "float64" (PrimitiveType Float64 Number) Nothing empty
    -- string types
    , TypeDeclaration "text" (PrimitiveType Text String) Nothing empty
    , TypeDeclaration "binary" (PrimitiveType Binary String) Nothing empty
    -- time types
    , TypeDeclaration
          "date" (PrimitiveType Date String) Nothing empty
    , TypeDeclaration
          "datetime" (PrimitiveType Datetime String) Nothing empty
    -- et cetera
    , TypeDeclaration "bool" (PrimitiveType Bool Boolean) Nothing empty
    , TypeDeclaration "uuid" (PrimitiveType Uuid String) Nothing empty
    , TypeDeclaration "uri" (PrimitiveType Uri String) Nothing empty
    ]

coreDocs :: Docs
coreDocs = [q|
Built-in types
==============

The core module is implicitly imported by every module so that built-in types
are available everywhere.

TBD.

|]
