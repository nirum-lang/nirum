{-# LANGUAGE RecordWildCards, TypeFamilies #-}
module Nirum.Targets.JavaScript ( JavaScript (..)
                                , compilePackage'
                                ) where

import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Aeson.Types (ToJSON, (.=), object, toJSON)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as LB
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Exts (IsList (toList))
import System.FilePath (joinPath)

import Nirum.Constructs.Identifier (toSnakeCaseText)
import Nirum.Constructs.ModulePath (ModulePath (..))

import Nirum.Package.Metadata ( Package (..)
                              , Target ( CompileError
                                       , CompileResult
                                       , compilePackage
                                       , parseTarget
                                       , showCompileError
                                       , targetName
                                       , toByteString
                                       )
                              , packageTarget
                              , stringField
                              )
import qualified Nirum.Package.ModuleSet as MS


data JavaScript = JavaScript { packageName :: T.Text }
    deriving (Eq, Ord, Show)

instance ToJSON JavaScript where
    toJSON JavaScript { .. } =
        object [ "name" .= packageName ]

newtype CodeBuilder = CodeBuilder { builder :: Builder }

instance Target JavaScript where
    type CompileResult JavaScript = CodeBuilder
    type CompileError JavaScript = ()
    targetName _ = "javascript"
    parseTarget table = do
        name' <- stringField "name" table
        return JavaScript { packageName = name' }
    compilePackage = compilePackage'
    showCompileError _ _e = ""
    toByteString _ = BSL.toStrict . encodeUtf8 . toLazyText . builder

compilePackage' :: Package JavaScript -> Map FilePath (Either () CodeBuilder)
compilePackage' package =
    M.fromList $
        files ++
        [("package.json", Right $ compilePackageMetadata package)]
  where
    toJavaScriptFilename :: ModulePath -> [FilePath]
    toJavaScriptFilename mp =
      case mp of
        ModulePath { .. } ->
          [ T.unpack (toSnakeCaseText i)
          | i <- toList path
          ]
          ++ [ f moduleName ]
        ModuleName { .. } ->
          [ f moduleName ]
      where
        f moduleName = T.unpack (toSnakeCaseText moduleName) ++ ".js"
    toFilename :: T.Text -> ModulePath -> FilePath
    toFilename sourceRootDirectory mp =
        joinPath $ T.unpack sourceRootDirectory : toJavaScriptFilename mp
    files :: [(FilePath, Either () CodeBuilder)]
    files = [ (toFilename "src" mp, Right $ CodeBuilder "")
            | mp <- MS.keys (modules package)
            ]


compilePackageMetadata :: Package JavaScript -> CodeBuilder
compilePackageMetadata = CodeBuilder . (`mappend` LB.singleton '\n') . encodePrettyToTextBuilder . packageTarget
