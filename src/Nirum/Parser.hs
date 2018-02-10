{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Nirum.Parser ( Parser
                    , ParseError
                    , aliasTypeDeclaration
                    , annotation
                    , annotationSet
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
                    , unboxedTypeDeclaration
                    , unionTypeDeclaration
                    ) where

import Control.Monad (void)
import Data.Void
import qualified System.IO as SIO

import Data.Map.Strict as Map hiding (foldl)
import Data.Set hiding (empty, foldl, fromList, map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec hiding (ParseError, parse)
import Text.Megaparsec.Char ( char
                            , eol
                            , noneOf
                            , spaceChar
                            , string
                            , string'
                            )
import qualified Text.Megaparsec.Error as E
import Text.Megaparsec.Char.Lexer (charLiteral)

import qualified Nirum.Constructs.Annotation as A
import Nirum.Constructs.Declaration (Declaration)
import Nirum.Constructs.Docs (Docs (Docs))
import Nirum.Constructs.DeclarationSet as DeclarationSet
import Nirum.Constructs.Identifier ( Identifier
                                   , identifierRule
                                   , reservedKeywords
                                   , toString
                                   )
import Nirum.Constructs.Module (Module (Module))
import Nirum.Constructs.ModulePath (ModulePath (ModulePath, ModuleName))
import Nirum.Constructs.Name (Name (Name))
import Nirum.Constructs.Service ( Method (Method)
                                , Parameter (Parameter)
                                , Service (Service)
                                )
import Nirum.Constructs.TypeDeclaration ( EnumMember (EnumMember)
                                        , Field (Field)
                                        , Tag (Tag)
                                        , Type ( Alias
                                               , EnumType
                                               , RecordType
                                               , UnboxedType
                                               , UnionType
                                               )
                                        , TypeDeclaration ( Import
                                                          , ServiceDeclaration
                                                          , TypeDeclaration
                                                          , serviceAnnotations
                                                          , typeAnnotations
                                                          )
                                        )
import Nirum.Constructs.TypeExpression ( TypeExpression ( ListModifier
                                                        , MapModifier
                                                        , OptionModifier
                                                        , SetModifier
                                                        , TypeIdentifier
                                                        )
                                       )

type Parser = Parsec Void T.Text
type ParseError = E.ParseError Char Void

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
    bareIdentifier :: Parser Identifier
    bareIdentifier = try $ do
        ident <- lookAhead identifierRule
        if ident `Data.Set.member` reservedKeywords
            then fail $ "\"" ++ toString ident ++ "\" is a reserved keyword; "
                        ++ "wrap it with backquotes to use it as a normal "
                        ++ "identifier (i.e. \"`" ++ toString ident ++ "`\")"
            else identifierRule
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

annotationArgumentValue :: Parser T.Text
annotationArgumentValue = do
    char '"'
    value <- manyTill charLiteral (char '"')
    return $ T.pack value

annotationArgument :: Parser (Identifier, T.Text)
annotationArgument = do
    arg <- identifier <?> "annotation parameter"
    spaces
    char '='
    spaces
    value <- annotationArgumentValue <?> "annotation argument value"
    return (arg, value)

annotation :: Parser A.Annotation
annotation = do
    char '@'
    spaces
    name' <- identifier
    spaces
    args' <- option Map.empty $ do
        char '('
        spaces
        args <- (`sepEndBy` char ',') $ do
            spaces
            a <- annotationArgument
            spaces
            return a
        spaces
        char ')'
        return $ Map.fromList args
    return $ A.Annotation name' args'

annotationSet :: Parser A.AnnotationSet
annotationSet = do
    annotations <- many $ do
        spaces
        a <- annotation
        spaces
        return a
    case A.fromList annotations of
        Right annotations' -> return annotations'
        Left (A.AnnotationNameDuplication _) -> fail "annotation name duplicate"

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
    comments <- sepEndBy1 (do
            char '#'
            void $ optional $ char ' '
            line <- many $ noneOf ("\r\n" :: String)
            return $ T.pack line
        ) (eol >> spaces) <?> "comments"
    return $ Docs $ T.unlines comments

annotationsWithDocs :: Monad m
                    => A.AnnotationSet
                    -> Maybe Docs
                    -> m A.AnnotationSet
annotationsWithDocs set' (Just docs') = A.insertDocs docs' set'
annotationsWithDocs set' Nothing = return set'

aliasTypeDeclaration :: Parser TypeDeclaration
aliasTypeDeclaration = do
    annotationSet' <- annotationSet <?> "type alias annotations"
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
    annotationSet'' <- annotationsWithDocs annotationSet' docs'
    return $ TypeDeclaration name' (Alias canonicalType) annotationSet''


unboxedTypeDeclaration :: Parser TypeDeclaration
unboxedTypeDeclaration = do
    annotationSet' <- annotationSet <?> "unboxed type annotations"
    string' "unboxed" <?> "unboxed type keyword"
    spaces
    typename <- identifier <?> "unboxed type name"
    let name' = Name typename typename
    spaces
    char '('
    spaces
    innerType <- typeExpression <?> "inner type of unboxed type"
    spaces
    char ')'
    spaces
    char ';'
    docs' <- optional $ try $ spaces >> (docs <?> "unboxed type docs")
    annotationSet'' <- annotationsWithDocs annotationSet' docs'
    return $ TypeDeclaration name' (UnboxedType innerType) annotationSet''

enumMember :: Parser EnumMember
enumMember = do
    annotationSet' <- annotationSet <?> "enum member annotations"
    spaces
    memberName <- name <?> "enum member name"
    spaces
    docs' <- optional $ do
        d <- docs <?> "enum member docs"
        spaces
        return d
    annotationSet'' <- annotationsWithDocs annotationSet' docs'
    return $ EnumMember memberName annotationSet''

handleNameDuplication :: Declaration a
                      => String -> [a]
                      -> (DeclarationSet a -> Parser b)
                      -> Parser b
handleNameDuplication label' declarations cont =
    case DeclarationSet.fromList declarations of
        Left (BehindNameDuplication (Name _ bname)) ->
            fail ("the behind " ++ label' ++ " name `" ++ toString bname ++
                  "` is duplicated")
        Left (FacialNameDuplication (Name fname _)) ->
            fail ("the facial " ++ label' ++ " name `" ++ toString fname ++
                  "` is duplicated")
        Right set -> cont set

enumTypeDeclaration :: Parser TypeDeclaration
enumTypeDeclaration = do
    annotationSet' <- annotationSet <?> "enum type annotations"
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
    annotationSet'' <- annotationsWithDocs annotationSet' docs'
    members <- (enumMember `sepBy1` (spaces >> char '|' >> spaces))
                   <?> "enum members"
    case DeclarationSet.fromList members of
        Left (BehindNameDuplication (Name _ bname)) ->
            fail ("the behind member name `" ++ toString bname ++
                  "` is duplicated")
        Left (FacialNameDuplication (Name fname _)) ->
            fail ("the facial member name `" ++ toString fname ++
                  "` is duplicated")
        Right memberSet -> do
            spaces
            char ';'
            return $ TypeDeclaration typename (EnumType memberSet)
                                     annotationSet''

fieldsOrParameters :: forall a . (String, String)
                   -> (Name -> TypeExpression -> A.AnnotationSet -> a)
                   -> Parser [a]
fieldsOrParameters (label', pluralLabel) make = do
    annotationSet' <- annotationSet <?> (label' ++ " annotations")
    spaces
    type' <- typeExpression <?> (label' ++ " type")
    spaces1
    name' <- name <?> (label' ++ " name")
    spaces
    let makeWithDocs = make name' type' . A.union annotationSet'
                                        . annotationsFromDocs
    followedByComma makeWithDocs <|> do
        d <- optional docs' <?> (label' ++ " docs")
        return [makeWithDocs d]

  where
    recur :: Parser [a]
    recur = fieldsOrParameters (label', pluralLabel) make
    followedByComma :: (Maybe Docs -> a) -> Parser [a]
    followedByComma makeWithDocs = do
        char ','
        spaces
        d <- optional docs' <?> (label' ++ " docs")
        rest <- option [] recur <?> ("rest of " ++ pluralLabel)
        return $ makeWithDocs d : rest
    docs' :: Parser Docs
    docs' = do
        d <- docs <?> (label' ++ " docs")
        spaces
        return d
    annotationsFromDocs :: Maybe Docs -> A.AnnotationSet
    annotationsFromDocs Nothing = A.empty
    annotationsFromDocs (Just d) = A.singleton $ A.docs d

fields :: Parser [Field]
fields = fieldsOrParameters ("label", "labels") Field

fieldSet :: Parser (DeclarationSet Field)
fieldSet = do
    fields' <- fields <?> "fields"
    handleNameDuplication "field" fields' return

recordTypeDeclaration :: Parser TypeDeclaration
recordTypeDeclaration = do
    annotationSet' <- annotationSet <?> "record type annotations"
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
    annotationSet'' <- annotationsWithDocs annotationSet' docs'
    return $ TypeDeclaration typename (RecordType fields') annotationSet''

tag :: Parser Tag
tag = do
    annotationSet' <- annotationSet <?> "union tag annotations"
    spaces
    tagName <- name <?> "union tag name"
    spaces
    paren <- optional $ char '('
    spaces
    frontDocs <- optional $ do
        d <- docs <?> "union tag docs"
        spaces
        return d
    fields' <- case paren of
        Just _ -> do
            spaces
            f <- fieldSet <?> "union tag fields"
            spaces
            char ')'
            return f
        Nothing -> return DeclarationSet.empty
    spaces
    docs' <- case frontDocs of
        d@(Just _) -> return d
        Nothing -> optional $ do
            d <- docs <?> "union tag docs"
            spaces
            return d
    annotationSet'' <- annotationsWithDocs annotationSet' docs'
    return $ Tag tagName fields' annotationSet''

unionTypeDeclaration :: Parser TypeDeclaration
unionTypeDeclaration = do
    annotationSet' <- annotationSet <?> "union type annotations"
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
    annotationSet'' <- annotationsWithDocs annotationSet' docs'
    handleNameDuplication "tag" tags' $ \ tagSet ->
        return $ TypeDeclaration typename (UnionType tagSet) annotationSet''

typeDeclaration :: Parser TypeDeclaration
typeDeclaration = do
    -- Preconsume the common prefix (annotations) to disambiguate
    -- the continued branches of parsers.
    spaces
    annotationSet' <- annotationSet <?> "type annotations"
    spaces
    typeDecl <- choice
        [ unless' ["union", "record", "enum", "unboxed"] aliasTypeDeclaration
        , unless' ["union", "record", "enum"] unboxedTypeDeclaration
        , unless' ["union", "record"] enumTypeDeclaration
        , unless' ["union"] recordTypeDeclaration
        , unionTypeDeclaration
        ] <?> "type declaration (e.g. enum, record, unboxed, union)"
    -- In theory, though it preconsumes annotationSet' before parsing typeDecl
    -- so that typeDecl itself has no annotations, to prepare for an
    -- unlikely situation (that I bet it'll never happen)
    -- unite the preconsumed annotationSet' with typeDecl's annotations
    -- (that must be empty).
    let annotations = A.union annotationSet' $ typeAnnotations typeDecl
    return $ typeDecl { typeAnnotations = annotations }
  where
    unless' :: [T.Text] -> Parser a -> Parser a
    unless' [] _ = fail "no candidates"  -- Must never happen
    unless' [s] p = notFollowedBy (string s) >> p
    unless' (x : xs) p = notFollowedBy (string x) >> unless' xs p

parameters :: Parser [Parameter]
parameters = fieldsOrParameters ("parameter", "parameters") Parameter

parameterSet :: Parser (DeclarationSet Parameter)
parameterSet = option DeclarationSet.empty $ try $ do
    params <- parameters <?> "method parameters"
    handleNameDuplication "parameter" params return

method :: Parser Method
method = do
    annotationSet' <- annotationSet <?> "service method annotation"

    returnType <- optional $ try $ do
        rt <- typeExpression <?> "method return type"
        spaces1
        notFollowedBy $ char '('
        return rt
    
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
    spaces
    errorType <- optional $ do
        string "throws" <?> "throws keyword"
        spaces
        e <- typeExpression <?> "method error type"
        spaces
        return e
    annotationSet'' <- annotationsWithDocs annotationSet' docs'
    return $ Method methodName params returnType errorType annotationSet''

methods :: Parser [Method]
methods = method `sepEndBy` try (spaces >> char ',' >> spaces)

methodSet :: Parser (DeclarationSet Method)
methodSet = do
    methods' <- methods <?> "service methods"
    handleNameDuplication "method" methods' return

serviceDeclaration :: Parser TypeDeclaration
serviceDeclaration = do
    annotationSet' <- annotationSet <?> "service annotation"
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
    annotationSet'' <- annotationsWithDocs annotationSet' docs'
    return $ ServiceDeclaration serviceName (Service methods') annotationSet''

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

importName :: Parser (Identifier, A.AnnotationSet)
importName = do
    aSet <- annotationSet <?> "import annotations"
    spaces
    iName <- identifier <?> "name to import"
    return (iName, aSet)

imports :: Parser [TypeDeclaration]
imports = do
    string' "import" <?> "import keyword"
    spaces
    path <- modulePath <?> "module path"
    spaces
    char '('
    spaces
    idents <- (importName >>= \i -> spaces >> return i)
        `sepEndBy1` (char ',' >> spaces)
        <?> "names to import"
    char ')'
    spaces
    char ';'
    return [Import path ident aSet | (ident, aSet) <- idents]


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
        typeDecl <- do
            -- Preconsume the common prefix (annotations) to disambiguate
            -- the continued branches of parsers.
            spaces
            annotationSet' <- annotationSet <?> "annotations"
            spaces
            decl <- choice [ notFollowedBy (string "service") >> typeDeclaration
                           , serviceDeclaration <?> "service declaration"
                           ]
            -- In theory, though it preconsumes annotationSet' before parsing
            -- decl so that decl itself has no annotations, to prepare for an
            -- unlikely situation (that I bet it'll never happen)
            -- unite the preconsumed annotationSet' with decl's annotations
            -- (that must be empty).
            return $ case decl of
                TypeDeclaration { typeAnnotations = set } ->
                    decl { typeAnnotations = A.union annotationSet' set }
                ServiceDeclaration { serviceAnnotations = set } ->
                    decl { serviceAnnotations = A.union annotationSet' set }
                _ -> decl  -- Never happen!
        spaces
        return typeDecl
    handleNameDuplication "type" (types ++ [i | l <- importLists, i <- l]) $
                          \ typeSet -> return $ Module typeSet docs'

file :: Parser Module
file = do
    mod' <- module'
    eof
    return mod'

parse :: FilePath -- ^ Source path (although it's only used for error message)
      -> T.Text   -- ^ Input source code
      -> Either ParseError Module
parse = runParser file

parseFile :: FilePath -- ^ Source path
          -> IO (Either ParseError Module)
parseFile path = do
    code <- SIO.withFile path SIO.ReadMode $ \ h -> do
        SIO.hSetEncoding h SIO.utf8_bom
        TIO.hGetContents h
    return $ runParser file path code

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
