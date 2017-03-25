{-# LANGUAGE OverloadedLists, QuasiQuotes #-}
module Nirum.Constructs.Module ( Module (Module, docs, types)
                               , coreModule
                               , coreModulePath
                               , coreTypes
                               , imports
                               ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (q)

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Annotation (empty)
import Nirum.Constructs.Declaration (Documented (docs))
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Docs (Docs)
import Nirum.Constructs.Identifier (Identifier)
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Constructs.TypeDeclaration ( JsonType (Boolean, Number, String)
                                        , PrimitiveTypeIdentifier ( Bigint
                                                                  , Binary
                                                                  , Bool
                                                                  , Date
                                                                  , Datetime
                                                                  , Decimal
                                                                  , Float32
                                                                  , Float64
                                                                  , Int32
                                                                  , Int64
                                                                  , Text
                                                                  , Uri
                                                                  , Uuid
                                                                  )
                                        , Type (PrimitiveType)
                                        , TypeDeclaration ( Import
                                                          , TypeDeclaration
                                                          , type'
                                                          , typeAnnotations
                                                          , typename
                                                          )
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

instance Documented Module where
    docs (Module _ docs') = docs'

imports :: Module -> M.Map ModulePath (S.Set Identifier)
imports (Module decls _) =
    M.fromListWith S.union [(p, [i]) | Import p i _ <- DS.toList decls]

coreModulePath :: ModulePath
coreModulePath = ["core"]

coreModule :: Module
coreModule = Module coreTypes $ Just coreDocs

coreTypes :: DS.DeclarationSet TypeDeclaration
coreTypes =
    -- number types
    [ decl' "bigint" Bigint String
    , decl' "decimal" Decimal String
    , decl' "int32" Int32 Number
    , decl' "int64" Int64 Number
    , decl' "float32" Float32 Number
    , decl' "float64" Float64 Number
    -- string types
    , decl' "text" Text String
    , decl' "binary" Binary String
    -- time types
    , decl' "date" Date String
    , decl' "datetime" Datetime String
    -- et cetera
    , decl' "bool" Bool Boolean
    , decl' "uuid" Uuid String
    , decl' "uri" Uri String
    ]
  where
    decl' name prim json =
        TypeDeclaration { typename = name
                        , type' = PrimitiveType prim json
                        , typeAnnotations = empty
                        }

coreDocs :: Docs
coreDocs = [q|
Built-in types
==============

The core module is implicitly imported by every module so that built-in types
are available everywhere.

TBD.

|]
