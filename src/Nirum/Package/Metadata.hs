{-# LANGUAGE QuasiQuotes #-}
module Nirum.Package.Metadata ( Metadata (Metadata, version)
                              , MetadataError ( FieldError
                                              , FieldTypeError
                                              , FieldValueError
                                              , FormatError
                                              )
                              , MetadataField
                              , MetadataFieldType
                              , metadataFilename
                              , metadataPath
                              , parseMetadata
                              , readFromPackage 
                              , readMetadata
                              ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.SemVer as SV
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))
import Text.InterpolatedString.Perl6 (qq)
import Text.Parsec.Error (ParseError)
import Text.Toml (parseTomlDoc)
import Text.Toml.Types (Node ( VArray
                             , VBoolean
                             , VDatetime
                             , VFloat
                             , VInteger
                             , VString
                             , VTable
                             , VTArray
                             )
                       , Table
                       )

-- | The filename of Nirum package metadata.
metadataFilename :: FilePath
metadataFilename = "package.toml"

data Metadata = Metadata { version :: SV.Version
                         } deriving (Eq, Ord, Show)
-- TODO: uri, dependencies

-- | Name of package.toml field.
type MetadataField = Text

-- | Typename of package.toml field e.g. @"string"@, @"array of 3 values"@.
type MetadataFieldType = Text

-- | Error related to parsing package.toml.
data MetadataError
    -- | A required field is missing.
    = FieldError MetadataField
    -- | A field has a value of incorrect type e.g. array for @version@ field.
    | FieldTypeError MetadataField MetadataFieldType MetadataFieldType
    -- | A field has a value of invalid format
    -- e.g. @"1/2/3"@ for @version@ field.
    | FieldValueError MetadataField String
    -- | The given package.toml file is not a valid TOML.
    | FormatError ParseError
    deriving (Eq, Show)

parseMetadata :: FilePath -> Text -> Either MetadataError Metadata
parseMetadata metadataPath' tomlText = do
    table <- case parseTomlDoc metadataPath' tomlText of
        Left e -> Left $ FormatError e
        Right t -> Right t
    version' <- versionField "version" table
    return Metadata { version = version' }

readMetadata :: FilePath -> IO (Either MetadataError Metadata)
readMetadata metadataPath' = do
    tomlText <- TIO.readFile metadataPath'
    return $ parseMetadata metadataPath' tomlText

metadataPath :: FilePath -> FilePath
metadataPath = (</> metadataFilename)

readFromPackage :: FilePath -> IO (Either MetadataError Metadata)
readFromPackage = readMetadata . metadataPath

printNode :: Node -> MetadataFieldType
printNode (VTable t) = [qq|table of {length t} items|]
printNode (VTArray a) = [qq|array of {length a} tables|]
printNode (VString s) = [qq|string ($s)|]
printNode (VInteger i) = [qq|integer ($i)|]
printNode (VFloat f) = [qq|float ($f)|]
printNode (VBoolean True) = "boolean (true)"
printNode (VBoolean False) = "boolean (false)"
printNode (VDatetime d) = [qq|datetime ($d)|]
printNode (VArray a) = [qq|array of {length a} values|]

field :: MetadataField -> Table -> Either MetadataError Node
field field' table =
    case HM.lookup field' table of
        Just node -> return node
        Nothing -> Left $ FieldError field'

typedField :: MetadataFieldType
           -> (Node -> Maybe v)
           -> MetadataField
           -> Table
           -> Either MetadataError v
typedField typename match field' table = do
    node <- field field' table
    case match node of
        Just value -> return value
        Nothing -> Left $ FieldTypeError field' typename $ printNode node

stringField :: MetadataField -> Table -> Either MetadataError Text
stringField = typedField "string" $ \n -> case n of
                                               VString s -> Just s
                                               _ -> Nothing

versionField :: MetadataField -> Table -> Either MetadataError SV.Version
versionField field' table = do
    s <- stringField field' table
    case SV.fromText s of
        Right v -> return v
        Left _ -> Left $ FieldValueError field' $
                    "expected a semver string (e.g. \"1.2.3\"), not " ++ show s
