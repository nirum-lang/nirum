{-# LANGUAGE FlexibleInstances, RecordWildCards, TypeFamilies #-}
module Nirum.Targets.JavaScript ( CodeBuilder
                                , CompileError' (..)
                                , JavaScript (..)
                                , compilePackage'
                                , methodDefinition
                                ) where

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

import qualified Nirum.CodeBuilder as CB
import Nirum.CodeBuilder (nest, runBuilder, writeLine)
import qualified Nirum.Constructs.Declaration as D
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Identifier ( Identifier
                                   , toCamelCaseText
                                   , toPascalCaseText
                                   , toSnakeCaseText
                                   )
import Nirum.Constructs.Module (Module (..))
import Nirum.Constructs.ModulePath (ModulePath (..))
import qualified Nirum.Constructs.Name as N
import Nirum.Constructs.TypeDeclaration ( Field (..)
                                        , Type (..)
                                        , TypeDeclaration (..)
                                        )

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

type CodeBuilder = CB.CodeBuilder JavaScript ()

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
    compile (mp, m) = Right $ Code $ snd $ runBuilder package mp () (compileModule m)

compilePackageMetadata :: Package JavaScript -> Code
compilePackageMetadata = Code . (`mappend` LB.singleton '\n') . encodePrettyToTextBuilder


compileModule :: Module -> CodeBuilder ()
compileModule Module {..} = mapM_ compileTypeDeclaration $ DS.toList types

compileTypeDeclaration :: TypeDeclaration -> CodeBuilder ()
compileTypeDeclaration td@TypeDeclaration { type' = RecordType fields } = do
    let name' = D.name td
    compileRecordConstructor name' fields
    writeLine ""
    compileRecordSerialize name' fields
    writeLine ""
    compileRecordDeserialize name' fields
compileTypeDeclaration _ = return ()

compileRecordConstructor :: N.Name -> DS.DeclarationSet Field -> CodeBuilder ()
compileRecordConstructor name fields = functionDefinition (toClassName name) [param] $ do
    let fields' = DS.toList fields
    writeLine "var errors = [];"
    mapM_ compileRecordTypeCheck fields'
    writeLine $ "if (errors.length > 0)" <+> P.lbrace
    nest 4 $ writeLine "throw new NirumError(errors);"
    writeLine P.rbrace
    mapM_ compileRecordInit fields'
    writeLine "Object.freeze(this);"
  where
    param = "values"
    values_ :: Field -> P.Doc
    values_ = dot (P.text $ T.unpack $ toCamelCaseText param) . toAttributeName . N.facialName . fieldName
    compileRecordTypeCheck :: Field -> CodeBuilder ()
    compileRecordTypeCheck field = do
        -- ty <- lookupType $ fieldType field
        writeLine $ "if" <+> P.parens (values_ field) <+> P.lbrace
        nest 4 $ writeLine $ "errors.push" <> P.parens P.empty <> P.semi
        writeLine P.rbrace
    compileRecordInit :: Field -> CodeBuilder ()
    compileRecordInit field =
        writeLine $ "this" `dot` toAttributeName (N.facialName $ fieldName field) <+> P.equals <+> values_ field <> P.semi

compileRecordSerialize :: N.Name -> DS.DeclarationSet Field -> CodeBuilder ()
compileRecordSerialize name fields = methodDefinition name "serialize" [] $ do
    writeLine $ "return" <+> P.lbrace
    nest 4 $ do
        writeLine $ "_type" <> P.colon <+> P.quotes (toDoc $ toSnakeCaseText $ N.behindName name)
        mapM_ field $ DS.toList fields
    writeLine $ P.rbrace <> P.semi
  where
    field :: Field -> CodeBuilder ()
    field f = writeLine $ P.quotes (toFieldName f) <> P.colon <+> "this" `dot` toFieldName f <> P.comma

compileRecordDeserialize :: N.Name -> DS.DeclarationSet Field -> CodeBuilder ()
compileRecordDeserialize name _fields = staticMethodDefinition name "deserialize" [] $
    writeLine $ "return" <+> "new" <+> toClassName name <> P.parens P.empty <> P.semi

functionDefinition'
    :: P.Doc  -- prefix
    -> P.Doc  -- end
    -> P.Doc  -- function name
    -> [Identifier]  -- parameters
    -> CodeBuilder ()  -- function body
    -> CodeBuilder ()
functionDefinition' prefix end name params body = do
    writeLine $ prefix <+> name <> P.parens params' <+> P.lbrace
    nest 4 body
    writeLine $ P.rbrace <> end
  where
    toParamName :: Identifier -> Doc
    toParamName = toAttributeName
    params' :: Doc
    params' = P.sep $ P.punctuate P.comma $ map toParamName params

functionDefinition :: P.Doc -> [Identifier] -> CodeBuilder () -> CodeBuilder ()
functionDefinition = functionDefinition' "function" P.empty

methodDefinition
    :: N.Name  -- class name
    -> Identifier  -- method name
    -> [Identifier]  -- parameters
    -> CodeBuilder ()  -- method body
    -> CodeBuilder ()
methodDefinition className name = functionDefinition' prefix P.semi (P.text "")
  where
    prefix = toClassName className `dot` "prototype" `dot` toAttributeName name <+> P.equals <+> "function"

staticMethodDefinition
    :: N.Name  -- class name
    -> Identifier  -- method name
    -> [Identifier]  -- parameters
    -> CodeBuilder ()  -- method body
    -> CodeBuilder ()
staticMethodDefinition className name = functionDefinition' prefix P.semi (P.text "")
  where
    prefix = toClassName className `dot` toAttributeName name <+> P.equals <+> "function"

toAttributeName :: Identifier -> Doc
toAttributeName = toDoc . toCamelCaseText

toFieldName :: Field -> Doc
toFieldName = toAttributeName . N.facialName . fieldName

toClassName :: N.Name -> Doc
toClassName = toDoc . toPascalCaseText . N.facialName

toDoc :: T.Text -> Doc
toDoc = P.text . T.unpack

dot :: P.Doc -> P.Doc -> P.Doc
a `dot` b = a <> P.char '.' <> b
