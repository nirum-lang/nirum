{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Nirum.Parser ( Parser
                    , aliasTypeDeclaration 
                    , boxedTypeDeclaration 
                    , docs
                    , enumTypeDeclaration
                    , file
                    , identifier
                    , listModifier
                    , mapModifier
                    , module'
                    , modulePath
                    , name
                    , optionModifier
                    , parse
                    , parseFile
                    , recordTypeDeclaration
                    , setModifier
                    , typeDeclaration
                    , typeExpression
                    , typeExpressionWithoutOptionModifier
                    , typeIdentifier
                    , unionTypeDeclaration
                    ) where

import Control.Monad (void)
import Data.List (foldl1')

import Data.Set (elems)
import qualified Data.Text as T
import Text.Megaparsec ( eof
                       , many
                       , notFollowedBy
                       , option
                       , optional
                       , parseFromFile
                       , runParser
                       , sepBy1
                       , sepEndBy1
                       , skipMany
                       , try
                       , unexpected
                       , (<|>)
                       , (<?>)
                       )
import Text.Megaparsec.Char (char, eol, noneOf, spaceChar, string, string')
import Text.Megaparsec.Error (ParseError)
import Text.Megaparsec.Text (Parser)

import Nirum.Constructs.Declaration (Docs(Docs))
import Nirum.Constructs.DeclarationSet ( DeclarationSet
                                       , NameDuplication( BehindNameDuplication
                                                        , FacialNameDuplication
                                                        )
                                       , empty
                                       , fromList
                                       )
import Nirum.Constructs.Identifier ( Identifier
                                   , identifierRule
                                   , reservedKeywords
                                   , toString
                                   )
import Nirum.Constructs.Module (Module(Module))
import Nirum.Constructs.ModulePath (ModulePath(ModulePath, ModuleName))
import Nirum.Constructs.Name (Name(Name))
import Nirum.Constructs.TypeDeclaration ( EnumMember(EnumMember)
                                        , Field(Field)
                                        , Tag(Tag)
                                        , Type( Alias
                                              , BoxedType
                                              , EnumType
                                              , RecordType
                                              , UnionType
                                              )
                                        , TypeDeclaration(TypeDeclaration)
                                        )
import Nirum.Constructs.TypeExpression ( TypeExpression( ListModifier
                                                       , MapModifier
                                                       , OptionModifier
                                                       , SetModifier
                                                       , TypeIdentifier
                                                       )
                                       )

comment :: Parser ()
comment = string "//" >> void (many $ noneOf "\n") <?> "comment"

spaces :: Parser ()
spaces = skipMany $ void spaceChar <|> comment

identifier :: Parser Identifier
identifier =
    quotedIdentifier <|> bareIdentifier <?> "identifier"
  where
    keywords :: Parser String
    keywords = foldl1' (<|>) $ map (string' . toString) $ elems reservedKeywords
    bareIdentifier :: Parser Identifier
    bareIdentifier = do
        notFollowedBy keywords
        identifierRule
    quotedIdentifier :: Parser Identifier
    quotedIdentifier = do
        char '`'
        identifier' <- identifierRule
        char '`'
        return identifier'

name :: Parser Name
name = do
    facialName <- identifier <?> "facial name"
    behindName <- option facialName $ try $ do
        spaces
        char '/'
        spaces
        identifier <?> "behind name"
    return $ Name facialName behindName

typeExpression :: Parser TypeExpression
typeExpression =
    try optionModifier <|> typeExpressionWithoutOptionModifier
                       <?> "type expression"

typeExpressionWithoutOptionModifier :: Parser TypeExpression
typeExpressionWithoutOptionModifier =
    try setModifier <|> listModifier <|> mapModifier <|> typeIdentifier

typeIdentifier :: Parser TypeExpression
typeIdentifier = do
    typeIdentifier' <- identifier <?> "type identifier"
    return $ TypeIdentifier typeIdentifier'

optionModifier :: Parser TypeExpression
optionModifier = do
    expr <- typeExpressionWithoutOptionModifier
    spaces
    char '?'
    return $ OptionModifier expr

setModifier :: Parser TypeExpression
setModifier = do
    char '{'
    spaces
    expr <- typeExpression <?> "element type of set type"
    spaces
    char '}'
    return $ SetModifier expr

listModifier :: Parser TypeExpression
listModifier = do
    char '['
    spaces
    expr <- typeExpression <?> "element type of list type"
    spaces
    char ']'
    return $ ListModifier expr

mapModifier :: Parser TypeExpression
mapModifier = do
    char '{'
    spaces
    key <- typeExpression <?> "key type of map type"
    spaces
    char ':'
    spaces
    value <- typeExpression <?> "value type of map type"
    spaces
    char '}'
    return $ MapModifier key value

docs :: Parser Docs
docs = do
    comments <- sepEndBy1 (do { char '#'
                              ; void $ optional $ char ' '
                              ; line <- many $ noneOf "\r\n"
                              ; return $ T.pack line
                              }) (eol >> spaces) <?> "comments"
    return $ Docs $ T.unlines comments

aliasTypeDeclaration :: Parser TypeDeclaration
aliasTypeDeclaration = do
    string' "type" <?> "type alias keyword"
    spaces
    typename <- identifier <?> "alias type name"
    let name' = Name typename typename
    spaces
    char '='
    spaces
    canonicalType <- typeExpression <?> "canonical type of alias"
    spaces
    char ';'
    docs' <- optional $ try $ spaces >> (docs <?> "type alias docs")
    return $ TypeDeclaration name' (Alias canonicalType) docs'

boxedTypeDeclaration :: Parser TypeDeclaration
boxedTypeDeclaration = do
    string' "boxed" <?> "boxed type keyword"
    spaces
    typename <- identifier <?> "boxed type name"
    let name' = Name typename typename
    spaces
    char '('
    spaces
    innerType <- typeExpression <?> "inner type of boxed type"
    spaces
    char ')'
    spaces
    char ';'
    docs' <- optional $ try $ spaces >> (docs <?> "boed type docs")
    return $ TypeDeclaration name' (BoxedType innerType) docs'

enumMember :: Parser EnumMember
enumMember = do
    memberName <- name <?> "enum member name"
    spaces
    docs' <- optional $ do
        d <- docs <?> "enum member docs"
        spaces
        return d
    return $ EnumMember memberName docs'

enumTypeDeclaration :: Parser TypeDeclaration
enumTypeDeclaration = do
    string "enum" <?> "enum keyword"
    spaces
    typename <- name <?> "enum type name"
    spaces
    char '='
    spaces
    docs' <- optional $ do
        d <- docs <?> "enum type docs"
        spaces
        return d
    members <- (enumMember `sepBy1` (spaces >> char '|' >> spaces))
                   <?> "enum members"
    case fromList members of
        Left (BehindNameDuplication (Name _ bname)) ->
            unexpected ("the behind member name `" ++ toString bname ++
                        "` is duplicated")
        Left (FacialNameDuplication (Name fname _)) ->
            unexpected ("the facial member name `" ++ toString fname ++
                        "` is duplicated")
        Right memberSet -> do
            spaces
            char ';'
            return $ TypeDeclaration typename (EnumType memberSet) docs'

fields :: Parser [Field]
fields = do
    fieldType <- typeExpression <?> "field type"
    spaces
    fieldName <- name <?> "field name"
    spaces
    let mkField = Field fieldName fieldType
    followedByComma mkField <|> do
        d <- optional docs' <?> "field docs"
        return [mkField d]
  where
    followedByComma :: (Maybe Docs -> Field) -> Parser [Field]
    followedByComma mkField = do
        char ','
        spaces
        d <- optional docs' <?> "field docs"
        rest <- option [] fields <?> "rest of fields"
        return $ mkField d : rest
    docs' :: Parser Docs
    docs' = do
        d <- docs <?> "field docs"
        spaces
        return d

fieldSet :: Parser (DeclarationSet Field)
fieldSet = do
    fields' <- fields <?> "fields"
    case fromList fields' of
        Left (BehindNameDuplication (Name _ bname)) ->
            unexpected ("the behind field name `" ++ toString bname ++
                        "` is duplicated")
        Left (FacialNameDuplication (Name fname _)) ->
            unexpected ("the facial field name `" ++ toString fname ++
                        "` is duplicated")
        Right set -> return set

recordTypeDeclaration :: Parser TypeDeclaration
recordTypeDeclaration = do
    string "record" <?> "record keyword"
    spaces
    typename <- name <?> "record type name"
    spaces
    char '('
    spaces
    docs' <- optional $ do
        d <- docs <?> "record type docs"
        spaces
        return d
    fields' <- fieldSet <?> "record fields"
    spaces
    char ')'
    spaces
    char ';'
    return $ TypeDeclaration typename (RecordType fields') docs'

tag :: Parser Tag
tag = do
    tagName <- name <?> "union tag name"
    spaces
    paren <- optional $ char '('
    fields' <- case paren of
        Just _ -> do { f <- fieldSet <?> "union tag fields"
                     ; char ')'
                     ; return f
                     }
        Nothing -> return empty
    docs' <- optional $ do
        d <- docs <?> "union tag docs"
        spaces
        return d
    return $ Tag tagName fields' docs'

unionTypeDeclaration :: Parser TypeDeclaration
unionTypeDeclaration = do
    string "union" <?> "union keyword"
    spaces
    typename <- name <?> "union type name"
    spaces
    docs' <- optional $ do
        d <- docs <?> "record type docs"
        spaces
        return d
    char '='
    spaces
    tags' <- (tag `sepBy1` try (spaces >> char '|' >> spaces))
             <?> "union tags"
    spaces
    char ';'
    case fromList tags' of
        Left (BehindNameDuplication (Name _ bname)) ->
            unexpected ("the behind tag name `" ++ toString bname ++
                        "` is duplicated")
        Left (FacialNameDuplication (Name fname _)) ->
            unexpected ("the facial tag name `" ++ toString fname ++
                        "` is duplicated")
        Right tagSet ->
            return $ TypeDeclaration typename (UnionType tagSet) docs'

typeDeclaration :: Parser TypeDeclaration
typeDeclaration =
    ( aliasTypeDeclaration <|>
      boxedTypeDeclaration <|>
      enumTypeDeclaration <|>
      recordTypeDeclaration <|>
      unionTypeDeclaration
    ) <?> "type declaration (e.g. boxed, enum, record, union)"

modulePath :: Parser ModulePath
modulePath = do
    idents <- sepBy1 (identifier <?> "module identifier")
                     (spaces >> char '.' >> spaces)
              <?> "module path"
    case makePath idents of
        Nothing -> unexpected "module path cannot be empty"
        Just path -> return path
  where
    makePath :: [Identifier] -> Maybe ModulePath
    makePath = foldl f Nothing
    f :: Maybe ModulePath -> Identifier -> Maybe ModulePath
    f Nothing i = Just $ ModuleName i
    f (Just p) i = Just $ ModulePath p i

module' :: Parser Module
module' = do
    spaces
    docs' <- optional $ do
        d <- docs <?> "module docs"
        spaces
        return d
    types <- many $ do
        typeDecl <- typeDeclaration
        spaces
        return typeDecl
    case fromList types of
        Left (BehindNameDuplication (Name _ bname)) ->
            unexpected ("the behind type name `" ++ toString bname ++
                        "` is duplicated")
        Left (FacialNameDuplication (Name fname _)) ->
            unexpected ("the facial type name `" ++ toString fname ++
                        "` is duplicated")
        Right typeSet -> return $ Module typeSet docs'

file :: Parser Module
file = do
    mod' <- module'
    eof
    return mod'

parse :: FilePath -- | Source path (although it's only used for error message)
      -> T.Text   -- | Input source code
      -> Either ParseError Module
parse = runParser file

parseFile :: FilePath -- | Source path
          -> IO (Either ParseError Module)
parseFile = parseFromFile file

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
