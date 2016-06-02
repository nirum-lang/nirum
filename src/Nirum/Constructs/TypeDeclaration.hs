{-# LANGUAGE OverloadedStrings #-}
module Nirum.Constructs.TypeDeclaration ( EnumMember(EnumMember)
                                        , Field(Field)
                                        , JsonType(..)
                                        , PrimitiveTypeIdentifier(..)
                                        , Tag(Tag)
                                        , Type(..)
                                        , TypeDeclaration(..)
                                        ) where

import Data.Maybe (isJust)
import Data.String (IsString(fromString))

import qualified Data.Text as T

import Nirum.Constructs (Construct(toCode))
import Nirum.Constructs.Declaration ( Declaration(..)
                                    , Docs(..)
                                    , toCodeWithPrefix
                                    )
import Nirum.Constructs.DeclarationSet (DeclarationSet, null', toList)
import Nirum.Constructs.Identifier (Identifier)
import Nirum.Constructs.Name (Name(Name))
import Nirum.Constructs.TypeExpression (TypeExpression)

data Type
    = Alias { canonicalType :: TypeExpression }
    | BoxedType { innerType :: TypeExpression }
    | EnumType { members :: DeclarationSet EnumMember }
    | RecordType { fields :: DeclarationSet Field }
    | UnionType { tags :: DeclarationSet Tag }
    | PrimitiveType { primitiveTypeIdentifier :: PrimitiveTypeIdentifier
                    , jsonType :: JsonType
                    }
    deriving (Eq, Ord, Show)

-- | Member of 'EnumType'.
data EnumMember = EnumMember Name (Maybe Docs) deriving (Eq, Ord, Show)

instance Construct EnumMember where
    toCode (EnumMember name' docs') = T.concat [ toCode name'
                                               , toCodeWithPrefix "\n" docs'
                                               ]

instance Declaration EnumMember where
    name (EnumMember name' _) = name'
    docs (EnumMember _ docs') = docs'

instance IsString EnumMember where
    fromString s = EnumMember (fromString s) Nothing

-- | Field of 'RecordType' and 'Tag'.
data Field = Field Name TypeExpression (Maybe Docs) deriving (Eq, Ord, Show)

instance Construct Field where
    toCode (Field name' typeExpr docs') = T.concat [ toCode typeExpr
                                                   , " "
                                                   , toCode name'
                                                   , ","
                                                   , toCodeWithPrefix "\n" docs'
                                                   ]

instance Declaration Field where
    name (Field name' _ _) = name'
    docs (Field _ _ docs') = docs'

-- | Tag of 'UnionType'.
data Tag = Tag Name (DeclarationSet Field) (Maybe Docs) deriving (Eq, Ord, Show)

instance Construct Tag where
    toCode (Tag name' fields' docs') =
        if null' fields'
        then T.concat [toCode name', toCodeWithPrefix "\n" docs']
        else T.concat [ toCode name'
                      , " (", fieldsCode, ")"
                      , toCodeWithPrefix "\n" docs'
                      ]
      where
        fieldsCode = T.intercalate " " $ map toCode $ toList fields'

instance Declaration Tag where
    name (Tag name' _ _) = name'
    docs (Tag _ _ docs') = docs'

-- | Primitive type identifiers.
data PrimitiveTypeIdentifier
    = Bigint | Decimal | Int32 | Int64 | Float32 | Float64 | Text | Binary
    | Date | Datetime | Bool | Uuid | Uri
    deriving (Eq, Ord, Show)

-- | Possible coded types of 'PrimitiveType' in JSON representation.
data JsonType = Boolean | Number | String deriving (Eq, Ord, Show)

-- Top-level 'Declaration' of type.
data TypeDeclaration
    = TypeDeclaration { typename :: Name
                      , type' :: Type
                      , typeDocs :: Maybe Docs
                      }
    deriving (Eq, Ord, Show)

instance Construct TypeDeclaration where
    toCode (TypeDeclaration name' (Alias cname) docs') =
        T.concat [ "type ", toCode name'
                 , " = ", toCode cname, ";"
                 , toCodeWithPrefix "\n" docs'
                 ]
    toCode (TypeDeclaration name' (BoxedType itype) docs') =
        T.concat [ "boxed ", toCode name'
                 , " (", toCode itype, ");"
                 , toCodeWithPrefix "\n" docs']
    toCode (TypeDeclaration name' (EnumType members') docs') =
        T.concat [ "enum ", toCode name'
                 , toCodeWithPrefix "\n    " docs'
                 , "\n    = ", T.replace "\n" "\n    " membersCode, "\n    ;"
                 ]
      where
        membersCode = T.intercalate "\n| " $ map toCode $ toList members'
    toCode (TypeDeclaration name' (RecordType fields') docs') =
        T.concat [ "record ", toCode name', " ("
                 , toCodeWithPrefix "\n    " docs'
                 , if isJust docs' then "\n" else ""
                 , T.replace "\n" "\n    " $ T.cons '\n' fieldsCode
                 , "\n);"
                 ]
      where
        fieldsCode = T.intercalate "\n" $ map toCode $ toList fields'
    toCode (TypeDeclaration name' (UnionType tags') docs') =
        T.concat [ "union ", nameCode
                 , toCodeWithPrefix "\n    " docs'
                 , "\n    = " , tagsCode
                 , "\n    ;"
                 ]
      where
        nameCode :: T.Text
        nameCode = toCode name'
        tagsCode :: T.Text
        tagsCode = T.intercalate "\n    | "
                                 [ T.replace "\n" "\n    " (toCode t)
                                 | t <- toList tags'
                                 ]
    toCode (TypeDeclaration name' (PrimitiveType typename' jsonType') docs') =
        T.concat [ "// primitive type `", toCode name', "`\n"
                 , "//     internal type identifier: ", showT typename', "\n"
                 , "//     coded to json ", showT jsonType', " type\n"
                 , docString docs'
                 ]
      where
        showT :: Show a => a -> T.Text
        showT = T.pack . show
        docString :: Maybe Docs -> T.Text
        docString Nothing = ""
        docString (Just (Docs d)) =
            T.concat ["\n// ", T.replace "\n" "\n// " $ T.stripEnd d, "\n"]

instance Declaration TypeDeclaration where
    name (TypeDeclaration name' _ _) = name'
    docs (TypeDeclaration _ _ docs') = docs'
