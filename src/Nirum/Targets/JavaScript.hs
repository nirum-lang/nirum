{-# LANGUAGE FlexibleInstances, RecordWildCards, TypeFamilies #-}
module Nirum.Targets.JavaScript ( JavaScript (..)
                                , CompileError' (..)
                                , compilePackage'
                                ) where

import Control.Monad (forM_)
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Aeson.Types (ToJSON, (.=), object, toJSON)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.SemVer as SV
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as LB
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Exts (IsList (toList))
import System.FilePath (joinPath)
import qualified Text.PrettyPrint as P
import Text.PrettyPrint (Doc, (<>), (<+>))

import Nirum.CodeBuilder (CodeBuilder, nest, runBuilder, writeLine)
import qualified Nirum.Constructs.Declaration as D
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Identifier (toCamelCaseText, toPascalCaseText, toSnakeCaseText)
import Nirum.Constructs.Module (Module (..))
import Nirum.Constructs.ModulePath (ModulePath (..))
import qualified Nirum.Constructs.Name as N
import Nirum.Constructs.TypeDeclaration (Field (..), Type (..), TypeDeclaration (..))

import Nirum.Package.Metadata ( Metadata (..)
                              , Package (..)
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


newtype JavaScript = JavaScript { packageName :: T.Text }
    deriving (Eq, Ord, Show)

instance ToJSON (Package JavaScript) where
    toJSON package = object [ "name" .= packageName
                            , "version" .= SV.toText version
                            ]
      where
        Metadata {..} = metadata package
        JavaScript {..} = packageTarget package

newtype Code = Code { builder :: Builder }
data CompileError' = CompileError'

instance Target JavaScript where
    type CompileResult JavaScript = Code
    type CompileError JavaScript = CompileError'
    targetName _ = "javascript"
    parseTarget table = do
        name' <- stringField "name" table
        return JavaScript { packageName = name' }
    compilePackage = compilePackage'
    showCompileError _ _e = ""
    toByteString _ = BSL.toStrict . encodeUtf8 . toLazyText . builder

compilePackage' :: Package JavaScript -> Map FilePath (Either CompileError' Code)
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
    files :: [(FilePath, Either CompileError' Code)]
    files = [ (toFilename "src" mp, compile (mp, m))
            | (mp, m) <- MS.toList (modules package)
            ]
    compile :: (ModulePath, Module) -> Either CompileError' Code
    compile (mp, m) = Right $ Code $ snd $ runBuilder package mp (compileModule m)

compilePackageMetadata :: Package JavaScript -> Code
compilePackageMetadata = Code . (`mappend` LB.singleton '\n') . encodePrettyToTextBuilder


compileModule :: Target t => Module -> CodeBuilder t ()
compileModule Module {..} = mapM_ compileTypeDeclaration $ DS.toList types

compileTypeDeclaration :: Target t => TypeDeclaration -> CodeBuilder t ()
compileTypeDeclaration td@TypeDeclaration {..} =
  case type' of
    RecordType {..} -> do
        writeLine $ "class" <+> toClassName (D.name td) <+> "{"
        nest 4 $ compileRecordBody fields
        writeLine "}"
    _ -> return ()
compileTypeDeclaration _ = return ()

compileRecordBody :: Target t => DS.DeclarationSet Field -> CodeBuilder t ()
compileRecordBody fields = do
    writeLine $ "constructor(values)" <+> "{"
    nest 4 $ do
        forM_ (DS.toList fields) $ \field ->
            writeLine $ "this" <> dot <> toFieldName field <+> "=" <+> "values" <> dot <> toFieldName field <> ";"
        writeLine "Object.freeze(this);"
    writeLine "}"


toFieldName :: Field -> Doc
toFieldName = P.text . T.unpack . toCamelCaseText . N.facialName . fieldName

toClassName :: N.Name -> Doc
toClassName = P.text . T.unpack . toPascalCaseText . N.facialName

dot :: P.Doc
dot = P.char '.'
