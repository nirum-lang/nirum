{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
module Nirum.Targets.Elm (Elm (Elm, repositoryUrl), Error) where

import Data.Maybe
import GHC.Exts (IsList (..))

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Map.Strict (Map)
import qualified Data.SemVer as SV
import Data.Text
import Text.URI

import Nirum.Constructs.Identifier
import Nirum.Constructs.ModulePath
import Nirum.Package.Metadata
import Nirum.Package.ModuleSet (keys)

newtype Elm = Elm { repositoryUrl :: URI } deriving (Eq, Ord, Show)

type Error = ()

sourceDirectory :: FilePath
sourceDirectory = "src"

toImportPath :: ModulePath -> Text
toImportPath = intercalate "." . fmap toPascalCaseText . toList

compilePackageJson :: Package Elm -> Value
compilePackageJson
    Package
        { metadata = Metadata { description = description'
                              , license = license'
                              , target = target'
                              , version = version'
                              }
        , modules = ms
        } =
    Object
        [ ("version", String $ SV.toText version')
        , ("summary", String $ fromMaybe "" description')
        , ("repository", String $ pack $ show $ repositoryUrl target')
        , ("license", String $ fromMaybe "" license')
        , ("source-directories", Array [String $ pack sourceDirectory])
        , ( "exposed-modules"
          , Array $ fromList $ (String . toImportPath) <$> keys ms
          )
        , ("dependencies", Object [("elm-lang/core", "5.0.0 <= v < 6.0.0")])
        , ("elm-version", "0.18.0 <= v < 0.19.0")
        ]

compilePackage' :: Package Elm -> Map FilePath (Either Error ByteString)
compilePackage' package =
    [("package.json", Right $ encodePretty $ compilePackageJson package)]

instance Target Elm where
    type CompileResult Elm = ByteString
    type CompileError Elm = Error
    targetName _ = "elm"
    parseTarget table = do
        repositoryUrl' <- uriField "repository" table
        return Elm
            { repositoryUrl = repositoryUrl'
            }
    compilePackage = compilePackage'
    showCompileError _ _ = "unexpected runtime error occurs"
    toByteString _ = toStrict
