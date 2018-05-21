module Nirum.Constructs.TypeDeclaration ( EnumMember (EnumMember)
                                        , Field ( Field
                                                , fieldAnnotations
                                                , fieldName
                                                , fieldType
                                                )
                                        , JsonType ( Boolean
                                                   , Number
                                                   , String
                                                   )
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
                                                                  , Url
                                                                  , Uuid
                                                                  )
                                        , Tag ( Tag
                                              , tagAnnotations
                                              , tagFields
                                              , tagName
                                              )
                                        , Type ( Alias
                                               , EnumType
                                               , PrimitiveType
                                               , RecordType
                                               , UnboxedType
                                               , UnionType
                                               , canonicalType
                                               , defaultTag
                                               , fields
                                               , innerType
                                               , jsonType
                                               , members
                                               , primitiveTypeIdentifier
                                               )
                                        , TypeDeclaration ( Import
                                                          , ServiceDeclaration
                                                          , TypeDeclaration
                                                          , importName
                                                          , modulePath
                                                          , service
                                                          , serviceAnnotations
                                                          , serviceName
                                                          , sourceName
                                                          , type'
                                                          , typeAnnotations
                                                          , typename
                                                          )
                                        , unionType
                                        , tags
                                        ) where

import Data.Maybe (isJust, maybeToList)
import Data.String (IsString (fromString))

import qualified Data.Text as T
import qualified Data.Set as S

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Annotation as A (AnnotationSet, empty, lookupDocs)
import Nirum.Constructs.Declaration ( Declaration (..)
                                    , Documented (docs)
                                    )
import Nirum.Constructs.Docs (Docs (Docs), toCodeWithPrefix)
import Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Identifier (Identifier)
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Constructs.Name (Name (..))
import Nirum.Constructs.Service ( Method
                                , Service (Service)
                                , methodDocs
                                )
import Nirum.Constructs.TypeExpression (TypeExpression)

data Type
    = Alias { canonicalType :: TypeExpression }
    | UnboxedType { innerType :: TypeExpression }
    | EnumType { members :: DeclarationSet EnumMember }
    | RecordType { fields :: DeclarationSet Field }
    | UnionType -- | Use 'unionType' instaed.
        { nondefaultTags :: DeclarationSet Tag -- This should not be exported.
        , defaultTag :: Maybe Tag
        }
    | PrimitiveType { primitiveTypeIdentifier :: PrimitiveTypeIdentifier
                    , jsonType :: JsonType
                    }
    deriving (Eq, Ord, Show)

tags :: Type -> DeclarationSet Tag
tags UnionType { nondefaultTags = tags', defaultTag = defTag } =
    case fromList $ toList tags' ++ maybeToList defTag of
        Right ts -> ts
        Left _ -> DS.empty -- must never happen!
tags _ = DS.empty

-- | Member of 'EnumType'.
data EnumMember = EnumMember Name AnnotationSet deriving (Eq, Ord, Show)

instance Construct EnumMember where
    toCode e@(EnumMember name' _) = T.concat [ toCode name'
                                             , toCodeWithPrefix "\n" (docs e)
                                             ]

instance Documented EnumMember

instance Declaration EnumMember where
    name (EnumMember name' _) = name'
    annotations (EnumMember _ anno') = anno'

instance IsString EnumMember where
    fromString s = EnumMember (fromString s) A.empty

-- | Field of 'RecordType' and 'Tag'.
data Field = Field { fieldName :: Name
                   , fieldType :: TypeExpression
                   , fieldAnnotations :: AnnotationSet
                   } deriving (Eq, Ord, Show)

instance Construct Field where
    toCode field@(Field name' typeExpr _) =
        T.concat [ toCode typeExpr
                 , " "
                 , toCode name'
                 , ","
                 , toCodeWithPrefix "\n" (docs field)
                 ]

instance Documented Field

instance Declaration Field where
    name (Field name' _ _) = name'
    annotations (Field _ _ anno') = anno'

-- | Tag of 'UnionType'.
data Tag = Tag { tagName :: Name
               , tagFields :: DeclarationSet Field
               , tagAnnotations :: AnnotationSet
               } deriving (Eq, Ord, Show)

-- | Create a 'UnionType'.
unionType :: [Tag] -> Maybe Tag -> Either NameDuplication Type
unionType t dt = case fromList t of
                     Right ts -> Right $
                         case dt of
                             Nothing -> UnionType ts Nothing
                             Just dt' -> UnionType (delete dt' ts) dt
                     Left a -> Left a

instance Construct Tag where
    toCode tag@(Tag name' fields' _) =
        if null' fields'
        then T.concat [toCode name', toCodeWithPrefix "\n" (docs tag)]
        else T.concat [ toCode name'
                      , " (", fieldsCode, ")"
                      , toCodeWithPrefix "\n" (docs tag)
                      ]
      where
        fieldsCode = T.intercalate " " $ map toCode $ toList fields'

instance Documented Tag

instance Declaration Tag where
    name (Tag name' _ _) = name'
    annotations (Tag _ _ anno') = anno'

-- | Primitive type identifiers.
data PrimitiveTypeIdentifier
    = Bigint | Decimal | Int32 | Int64 | Float32 | Float64 | Text | Binary
    | Date | Datetime | Bool | Uuid | Url
    deriving (Eq, Ord, Show)

-- | Possible coded types of 'PrimitiveType' in JSON representation.
data JsonType = Boolean | Number | String deriving (Eq, Ord, Show)

-- Top-level 'Declaration' of type.
data TypeDeclaration
    = TypeDeclaration { typename :: Name
                      , type' :: Type
                      , typeAnnotations :: AnnotationSet
                      }
    | ServiceDeclaration { serviceName :: Name
                         , service :: Service
                         , serviceAnnotations :: AnnotationSet
                         }
    | Import { modulePath :: ModulePath
             , importName :: Identifier
             , sourceName :: Identifier
             , importAnnotations :: AnnotationSet
             }
    deriving (Eq, Ord, Show)

instance Construct TypeDeclaration where
    toCode (TypeDeclaration name' (Alias cname) annotationSet') =
        T.concat [ toCode annotationSet'
                 , "type ", toCode name'
                 , " = ", toCode cname, ";"
                 , toCodeWithPrefix "\n" (A.lookupDocs annotationSet')
                 ]
    toCode (TypeDeclaration name' (UnboxedType itype) annotationSet') =
        T.concat [ toCode annotationSet'
                 , "unboxed ", toCode name'
                 , " (", toCode itype, ");"
                 , toCodeWithPrefix "\n" (A.lookupDocs annotationSet')]
    toCode (TypeDeclaration name' (EnumType members') annotationSet') =
        T.concat [ toCode annotationSet'
                 , "enum ", toCode name'
                 , toCodeWithPrefix "\n    " (A.lookupDocs annotationSet')
                 , "\n    = ", T.replace "\n" "\n    " membersCode, "\n    ;"
                 ]
      where
        membersCode = T.intercalate "\n| " $ map toCode $ toList members'
    toCode (TypeDeclaration name' (RecordType fields') annotationSet') =
        T.concat [ toCode annotationSet'
                 , "record ", toCode name', " ("
                 , toCodeWithPrefix "\n    " docs'
                 , if isJust docs' then "\n" else ""
                 , T.replace "\n" "\n    " $ T.cons '\n' fieldsCode
                 , "\n);"
                 ]
      where
        fieldsCode = T.intercalate "\n" $ map toCode $ toList fields'
        docs' = A.lookupDocs annotationSet'
    toCode (TypeDeclaration name' (UnionType tags' defaultTag') as') =
        T.concat [ toCode as'
                 , "union ", nameCode
                 , toCodeWithPrefix "\n    " (A.lookupDocs as')
                 , "\n    = " , tagsCode
                 , "\n    ;"
                 ]
      where
        nameCode :: T.Text
        nameCode = toCode name'
        tagsCode :: T.Text
        tagsCode = T.intercalate "\n    | "
                                 [ T.replace "\n" "\n    " $
                                             if defaultTag' == Just t
                                             then T.append "default " (toCode t)
                                             else toCode t
                                 | t <- maybeToList defaultTag' ++ toList tags'
                                 ]
    toCode (TypeDeclaration name'
                            (PrimitiveType typename' jsonType')
                            as') =
        T.concat [ toCode as'
                 , "// primitive type `", toCode name', "`\n"
                 , "//     internal type identifier: ", showT typename', "\n"
                 , "//     coded to json ", showT jsonType', " type\n"
                 , docString (A.lookupDocs as')
                 ]
      where
        showT :: Show a => a -> T.Text
        showT = T.pack . show
        docString :: Maybe Docs -> T.Text
        docString Nothing = ""
        docString (Just (Docs d)) =
            T.concat ["\n// ", T.replace "\n" "\n// " $ T.stripEnd d, "\n"]
    toCode (ServiceDeclaration name' (Service methods) annotations') =
        T.concat [ toCode annotations'
                 , "service "
                 , toCode name'
                 , " ("
                 , toCodeWithPrefix "\n    " docs'
                 , case (docs', map methodDocs methods') of
                      (_, []) -> ""
                      (Nothing, [Nothing]) -> ""
                      _ -> "\n    "
                 , if length methods' > 1
                   then methodsText
                   else T.dropWhileEnd (== ',') (T.strip methodsText)
                 , case docs' of
                       Nothing -> ""
                       Just _ -> "\n"
                 , ");"
                 ]
      where
        methods' :: [Method]
        methods' = toList methods
        methodsText :: T.Text
        methodsText = T.intercalate "\n" $ map toCode methods'
        docs' :: Maybe Docs
        docs' = A.lookupDocs annotations'
    toCode (Import path iName sName aSet) = T.concat
        [ "import "
        , toCode path
        , " ("
        , toCode aSet
        , if iName == sName
             then toCode iName
             else T.concat [ toCode sName, " as ", toCode iName ]
        , ");\n"
        ]

instance Documented TypeDeclaration

instance Declaration TypeDeclaration where
    name TypeDeclaration { typename = name' } = name'
    name ServiceDeclaration { serviceName = name' } = name'
    name Import { importName = id' } = Name id' id'
    extraPublicNames TypeDeclaration { type' = EnumType { members = ms } } =
        S.fromList [facialName mName | EnumMember mName _ <- toList ms]
    extraPublicNames TypeDeclaration { type' = unionType'@UnionType {} } =
        S.fromList $ map (facialName . tagName) $ toList $ tags unionType'
    extraPublicNames _ = S.empty
    annotations TypeDeclaration { typeAnnotations = anno' } = anno'
    annotations ServiceDeclaration { serviceAnnotations = anno' } = anno'
    annotations Import { importAnnotations = anno' } = anno'
