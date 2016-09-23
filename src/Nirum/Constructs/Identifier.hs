{-# LANGUAGE OverloadedLists #-}
module Nirum.Constructs.Identifier ( Identifier
                                   , fromString
                                   , fromText
                                   , identifierRule
                                   , normalize
                                   , reservedKeywords
                                   , show
                                   , toCode
                                   , tokens
                                   , toCamelCaseText
                                   , toNormalizedString
                                   , toNormalizedText
                                   , toString
                                   , toSnakeCaseText
                                   , toText
                                   , toPascalCaseText
                                   , toLispCaseText
                                   , (==)
                                   ) where

import Data.Char (toLower, toUpper)
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))

import qualified Data.Text as T
import qualified Data.Set as S
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (oneOf, satisfy)
import Text.Megaparsec.Text (Parser)

import Nirum.Constructs (Construct(toCode))

-- | Case-insensitive identifier.  It also doesn't distinguish hyphens
-- from underscores.
--
-- It has more restrict rules than the most of programming languages:
--
-- * It can't start with digits or hyphens/underscores.
-- * Hyphens/underscores can't continuously appear more than once.
-- * Only roman alphabets, Arabic numerals, hyphens and underscores
--   are allowed.
-- 
-- These rules are for portability between various programming languages.
-- For example, @BOOK_CATEGORY@ and @Book-Category@ are all normalized
-- to @book-category@, and it can be translated to:
-- 
-- [snake_case] @book_category@
-- [camelCase] @bookCategory@
-- [PascalCase] @BookCategory@
-- [lisp-case] @book-category@
data Identifier = Identifier T.Text deriving (Show)

reservedKeywords :: S.Set Identifier
reservedKeywords = [ "enum"
                   , "record"
                   , "service"
                   , "throws"
                   , "type"
                   , "unboxed"
                   , "union"
                   ]

identifierRule :: Parser Identifier
identifierRule = do
    firstChar <- satisfy isAlpha
    restChars <- P.many $ satisfy isAlnum
    restWords <- P.many $ do
        sep <- oneOf ("-_" :: String)
        chars <- P.some $ satisfy isAlnum
        return $ T.pack $ sep:chars
    return $ Identifier $ T.concat $ T.pack (firstChar : restChars) : restWords
  where
    isAlpha :: Char -> Bool
    isAlpha c = 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'
    isDigit :: Char -> Bool
    isDigit c = '0' <= c && c <= '9'
    isAlnum :: Char -> Bool
    isAlnum c = isAlpha c || isDigit c

-- | Constructs a 'Identifier' value from the given identifier string.
-- It could return 'Nothing' if the given identifier is invalid.
fromText :: T.Text -> Maybe Identifier
fromText text =
    case P.parse rule "" text of
        Right ident -> Just ident
        Left _ -> Nothing
  where
    rule :: Parser Identifier
    rule = do
        identifier' <- identifierRule
        _ <- P.eof
        return identifier'

normalize :: Identifier -> Identifier
normalize (Identifier i) =
    Identifier $ T.map (\c -> if c == '_' then '-' else toLower c) i

toText :: Identifier -> T.Text
toText (Identifier text) = text

toNormalizedText :: Identifier -> T.Text
toNormalizedText = toText . normalize

tokens :: Identifier -> [T.Text]
tokens ident = T.split (== '-') $ toText $ normalize ident

instance Eq Identifier where
    a@(Identifier _) == b@(Identifier _) =
        toNormalizedText a == toNormalizedText b

instance Ord Identifier where
    compare a@(Identifier _) b@(Identifier _) =
        compare (toNormalizedText a) (toNormalizedText b)

instance Construct Identifier where
    toCode ident
        | ident `S.member` reservedKeywords = '`' `T.cons` text `T.snoc` '`'
        | otherwise = text
      where
        text = toText ident

instance IsString Identifier where
    fromString string = fromMaybe (error $ "invalid identifier: " ++ string) $
                                  fromText (T.pack string)

toString :: Identifier -> String
toString = T.unpack . toText

toNormalizedString :: Identifier -> String
toNormalizedString = T.unpack . toNormalizedText

toPascalCaseText :: Identifier -> T.Text
toPascalCaseText identifier =
    T.concat $ fmap makeFirstUpper (tokens identifier)
  where
    makeFirstUpper :: T.Text -> T.Text
    makeFirstUpper t = toUpper (T.head t) `T.cons` T.tail t

toCamelCaseText :: Identifier -> T.Text
toCamelCaseText identifier =
    toLower (T.head pascalCased) `T.cons` T.tail pascalCased
  where
    pascalCased :: T.Text
    pascalCased = toPascalCaseText identifier

toSnakeCaseText :: Identifier -> T.Text
toSnakeCaseText identifier = T.intercalate "_" $ tokens identifier

toLispCaseText :: Identifier -> T.Text
toLispCaseText = toNormalizedText
