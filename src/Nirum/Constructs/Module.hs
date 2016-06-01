{-# LANGUAGE OverloadedLists, OverloadedStrings, QuasiQuotes  #-}
module Nirum.Constructs.Module (Module(..), coreModule) where

import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (q)

import Nirum.Constructs (Construct(toCode))
import Nirum.Constructs.Declaration (Docs)
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.TypeDeclaration ( JsonType(..)
                                        , PrimitiveTypeIdentifier(..)
                                        , Type(..)
                                        , TypeDeclaration(..)
                                        )

data Module = Module { types :: DS.DeclarationSet TypeDeclaration
                     , docs :: Maybe Docs
                     } deriving (Eq, Ord, Show)

instance Construct Module where
    toCode (Module types' docs') =
        T.concat [ maybe "" ((`T.snoc` '\n') . toCode) docs'
                 , "\n"
                 , T.intercalate "\n\n" $ map toCode $ DS.toList types'
                 , "\n"
                 ]

coreModule :: Module
coreModule = Module coreTypes $ Just coreDocs

coreTypes :: DS.DeclarationSet TypeDeclaration
coreTypes =
    -- number types
    [ TypeDeclaration "bigint" (PrimitiveType Bigint String) Nothing
    , TypeDeclaration "decimal" (PrimitiveType Decimal String) Nothing
    , TypeDeclaration "int32" (PrimitiveType Int32 Number) Nothing
    , TypeDeclaration "int64" (PrimitiveType Int64 Number) Nothing
    , TypeDeclaration "float32" (PrimitiveType Float32 Number) Nothing
    , TypeDeclaration "float64" (PrimitiveType Float64 Number) Nothing
    -- string types
    , TypeDeclaration "text" (PrimitiveType Text String) Nothing
    , TypeDeclaration "binary" (PrimitiveType Binary String) Nothing
    -- time types
    , TypeDeclaration "date" (PrimitiveType Date String) Nothing
    , TypeDeclaration "datetime" (PrimitiveType Datetime String) Nothing
    -- et cetera
    , TypeDeclaration "bool" (PrimitiveType Bool Boolean) Nothing
    , TypeDeclaration "uuid" (PrimitiveType Uuid String) Nothing
    , TypeDeclaration "uri" (PrimitiveType Uri String) Nothing
    ]

coreDocs :: Docs
coreDocs = [q|
Built-in types
==============

The core module is implicitly imported by every module so that built-in types
are available everywhere.

TBD.

|]
