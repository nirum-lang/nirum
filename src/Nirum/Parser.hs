{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Nirum.Parser ( Parser
                    , ParseError
                    , aliasTypeDeclaration
                    , annotation
                    , boxedTypeDeclaration
                    , docs
                    , enumTypeDeclaration
                    , file
                    , handleNameDuplication
                    , identifier
                    , imports
                    , listModifier
                    , mapModifier
                    , method
                    , module'
                    , modulePath
                    , name
                    , optionModifier
                    , parse
                    , parseFile
                    , recordTypeDeclaration
                    , serviceDeclaration
                    , setModifier
                    , typeDeclaration
                    , typeExpression
                    , typeExpressionWithoutOptionModifier
                    , typeIdentifier
                    , unionTypeDeclaration
                    ) where

import Control.Monad (void)
import Data.List (foldl1')
import Prelude hiding (readFile)

import Data.Set (elems)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Text.Megaparsec ( Token
                       , eof
                       , many
                       , manyTill
                       , notFollowedBy
                       , option
                       , optional
                       , runParser
                       , sepBy1
                       , sepEndBy
                       , sepEndBy1
                       , skipMany
                       , skipSome
                       , try
                       , (<|>)
                       , (<?>)
                       )
import Text.Megaparsec.Char (char
                            , eol
                            , noneOf
                            , spaceChar
                            , string
                            , string'
                            )
import qualified Text.Megaparsec.Error as E
import Text.Megaparsec.Text (Parser)
import Text.Megaparsec.Lexer (charLiteral)

import Nirum.Constructs.Annotation (Annotation(Annotation))
import Nirum.Constructs.Declaration (Declaration, Docs(Docs))
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
import Nirum.Constructs.Service ( Method(Method)
                                , Parameter(Parameter)
                                , Service(Service)
                                )
import Nirum.Constructs.TypeDeclaration ( EnumMember(EnumMember)
                                        , Field(Field)
                                        , Tag(Tag)
                                        , Type( Alias
                                              , BoxedType
                                              , EnumType
                                              , RecordType
                                              , UnionType
                                              )
                                        , TypeDeclaration( Import
                                                         , ServiceDeclaration
                                                         , TypeDeclaration
                                                         )
                                        )
import Nirum.Constructs.TypeExpression ( TypeExpression( ListModifier
                                                       , MapModifier
                                                       , OptionModifier
                                                       , SetModifier
                                                       , TypeIdentifier
                                                       )
                                       )

type ParseError = E.ParseError (Token T.Text) E.Dec

comment :: Parser ()
comment = string "//" >> void (many $ noneOf ("\n" :: String)) <?> "comment"

spaces :: Parser ()
spaces = skipMany $ void spaceChar <|> comment

spaces1 :: Parser ()
spaces1 = skipSome $ void spaceChar <|> comment

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

annotation :: Parser Annotation
annotation = do
    char '['
    spaces
    name' <- identifier
    spaces
    char ':'
    spaces
    metadata <- (char '"' >> manyTill charLiteral (char '"'))
                <?> "annotation metadata"
    spaces
    char ']'
    return $ Annotation name' $ T.pack metadata


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
                              ; line <- many $ noneOf ("\r\n" :: String)
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

handleNameDuplication :: Declaration a
                      => String -> [a]
                      -> (DeclarationSet a -> Parser b)
                      -> Parser b
handleNameDuplication label declarations cont =
    case fromList declarations of
        Left (BehindNameDuplication (Name _ bname)) ->
            fail ("the behind " ++ label ++ " name `" ++ toString bname ++
                  "` is duplicated")
        Left (FacialNameDuplication (Name fname _)) ->
            fail ("the facial " ++ label ++ " name `" ++ toString fname ++
                  "` is duplicated")
        Right set -> cont set

enumTypeDeclaration :: Parser TypeDeclaration
enumTypeDeclaration = do
    string "enum" <?> "enum keyword"
    spaces
    typename <- name <?> "enum type name"
    spaces
    frontDocs <- optional $ do
        d <- docs <?> "enum type docs"
        spaces
        return d
    char '='
    spaces
    docs' <- case frontDocs of
        d@(Just _) -> return d
        Nothing -> optional $ do
            d <- docs <?> "enum type docs"
            spaces
            return d
    members <- (enumMember `sepBy1` (spaces >> char '|' >> spaces))
                   <?> "enum members"
    case fromList members of
        Left (BehindNameDuplication (Name _ bname)) ->
            fail ("the behind member name `" ++ toString bname ++
                  "` is duplicated")
        Left (FacialNameDuplication (Name fname _)) ->
            fail ("the facial member name `" ++ toString fname ++
                  "` is duplicated")
        Right memberSet -> do
            spaces
            char ';'
            return $ TypeDeclaration typename (EnumType memberSet) docs'

fieldsOrParameters :: forall a. (String, String)
                   -> (Name -> TypeExpression -> Maybe Docs -> a)
                   -> Parser [a]
fieldsOrParameters (label, pluralLabel) make = do
    type' <- typeExpression <?> (label ++ " type")
    spaces1
    name' <- name <?> (label ++ " name")
    spaces
    let makeWithDocs = make name' type'
    followedByComma makeWithDocs <|> do
        d <- optional docs' <?> (label ++ " docs")
        return [makeWithDocs d]
  where
    recur :: Parser [a]
    recur = fieldsOrParameters (label, pluralLabel) make
    followedByComma :: (Maybe Docs -> a) -> Parser [a]
    followedByComma makeWithDocs = do
        char ','
        spaces
        d <- optional docs' <?> (label ++ " docs")
        rest <- option [] recur <?> ("rest of " ++ pluralLabel)
        return $ makeWithDocs d : rest
    docs' :: Parser Docs
    docs' = do
        d <- docs <?> (label ++ " docs")
        spaces
        return d

fields :: Parser [Field]
fields = fieldsOrParameters ("label", "labels") Field

fieldSet :: Parser (DeclarationSet Field)
fieldSet = do
    fields' <- fields <?> "fields"
    handleNameDuplication "field" fields' return

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
        d <- docs <?> "union type docs"
        spaces
        return d
    char '='
    spaces
    tags' <- (tag `sepBy1` try (spaces >> char '|' >> spaces))
             <?> "union tags"
    spaces
    char ';'
    handleNameDuplication "tag" tags' $ \tagSet ->
        return $ TypeDeclaration typename (UnionType tagSet) docs'

typeDeclaration :: Parser TypeDeclaration
typeDeclaration =
    ( aliasTypeDeclaration <|>
      boxedTypeDeclaration <|>
      enumTypeDeclaration <|>
      recordTypeDeclaration <|>
      unionTypeDeclaration
    ) <?> "type declaration (e.g. boxed, enum, record, union)"

parameters :: Parser [Parameter]
parameters = fieldsOrParameters ("parameter", "parameters") Parameter

parameterSet :: Parser (DeclarationSet Parameter)
parameterSet = option empty $ try $ do
    params <- parameters <?> "method parameters"
    handleNameDuplication "parameter" params return

method :: Parser Method
method = do
    returnType <- typeExpression <?> "method return type"
    spaces1
    methodName <- name <?> "method name"
    spaces
    char '('
    spaces
    docs' <- optional $ do
        d <- docs <?> "method docs"
        spaces
        return d
    params <- parameterSet
    spaces
    char ')'
    return $ Method methodName params returnType docs'

methods :: Parser [Method]
methods = method `sepEndBy` try (spaces >> char ',' >> spaces)

methodSet :: Parser (DeclarationSet Method)
methodSet = do
    methods' <- methods <?> "service methods"
    handleNameDuplication "method" methods' return

serviceDeclaration :: Parser TypeDeclaration
serviceDeclaration = do
    string "service" <?> "service keyword"
    spaces
    serviceName <- name <?> "service name"
    spaces
    char '('
    spaces
    docs' <- optional $ do
        d <- docs <?> "service docs"
        spaces
        return d
    methods' <- methodSet <?> "service methods"
    spaces
    char ')'
    spaces
    char ';'
    return $ ServiceDeclaration serviceName (Service methods') docs'

modulePath :: Parser ModulePath
modulePath = do
    idents <- sepBy1 (identifier <?> "module identifier")
                     (try (spaces >> char '.' >> spaces))
              <?> "module path"
    case makePath idents of
        Nothing -> fail "module path cannot be empty"
        Just path -> return path
  where
    makePath :: [Identifier] -> Maybe ModulePath
    makePath = foldl f Nothing
    f :: Maybe ModulePath -> Identifier -> Maybe ModulePath
    f Nothing i = Just $ ModuleName i
    f (Just p) i = Just $ ModulePath p i

imports :: Parser [TypeDeclaration]
imports = do
    string' "import" <?> "import keyword"
    spaces
    path <- modulePath <?> "module path"
    spaces
    char '('
    spaces
    idents <- sepBy1 (identifier <?> "name to import")
                     (spaces >> char ',' >> spaces)
              <?> "names to import"
    spaces
    char ')'
    spaces
    char ';'
    return [Import path ident | ident <- idents]


module' :: Parser Module
module' = do
    spaces
    docs' <- optional $ do
        d <- docs <?> "module docs"
        spaces
        return d
    spaces
    importLists <- many $ do
        importList <- imports
        spaces
        return importList
    types <- many $ do
        typeDecl <- typeDeclaration <|>
                    (serviceDeclaration <?> "service declaration")
        spaces
        return typeDecl
    handleNameDuplication "type" (types ++ [i | l <- importLists, i <- l]) $
                          \typeSet -> return $ Module typeSet docs'

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
parseFile path = do
    code <- readFile path
    return $ runParser file path code

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
