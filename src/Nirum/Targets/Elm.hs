{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Nirum.Targets.Elm (Elm (Elm, repositoryUrl), Error) where

import Data.List (sort)
import Data.Maybe
import GHC.Exts (IsList (..))
import Prelude hiding (concat)

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map, insert)
import qualified Data.SemVer as SV
import Data.Set (Set, unions)
import Data.Text hiding (zip)
import Text.Blaze (Markup)
import Text.Blaze.Renderer.Utf8
import Text.Heterocephalus (compileText)

import Nirum.Constructs.Declaration
import Nirum.Constructs.Docs as Docs
import Nirum.Constructs.Identifier
import Nirum.Constructs.ModulePath
import Nirum.Constructs.Name
import Nirum.Constructs.TypeDeclaration hiding (String, modulePath)
import Nirum.Package.Metadata hiding (name)
import Nirum.Package.ModuleSet (keys)
import Nirum.Targets.Elm.CodeGen
import Nirum.Targets.Elm.Types
import Nirum.TypeInstance.BoundModule

compileModule :: BoundModule Elm -> Either Error (Set ElmLibrary, Markup)
compileModule boundModule = do
    let (compiledTypesResult, ctx) =
            runCodeGen' $ mapM (compileType boundModule) types'
    let CodeGenContext { imports = is, dependencies = deps } = ctx
    compiledTypes <- compiledTypesResult
    return $ (,) deps [compileText|
module #{toImportPath modulePath'}
    exposing
%{ forall (i, typename) <- enumerate exportNames }
%{ if i < 1 }
        ( #{typename}
%{ else }
        , #{typename}
%{ endif }
%{ endforall }
        )

{-| #{maybe (toImportPath modulePath') Docs.toText (docs boundModule)}

@docs #{intercalate ", " exportNames}
-}

%{ forall (modulePath, alias) <- toList is }
import #{modulePath} as #{alias}
%{ endforall }

%{ forall (compiled, type') <- zip compiledTypes types' }
%{ case type' }
%{ of TypeDeclaration _ _ _ }
{-| #{maybe "" Docs.toText (docs type')}
-}
%{ of _ }
%{ endcase }
#{compiled}
%{ endforall }
|]
  where
    modulePath' :: ModulePath
    modulePath' = modulePath boundModule
    types' :: [TypeDeclaration]
    types' = toList $ boundTypes boundModule
    typeName :: TypeDeclaration -> Text
    typeName = toPascalCaseText . facialName . name
    exportNames :: [Text]
    exportNames = sort [typeName t | t@TypeDeclaration {} <- types']

toDependency :: ElmLibrary -> (Text, SV.Version, SV.Version, Maybe Text)
toDependency library = case library of
    ElmBytes ->
        ( "spisemisu/elm-bytes"
        , SV.version 1 1 0 [] []
        , SV.version 2 0 0 [] []
        , Nothing
        )
    ElmDecimal ->
        ( "prikhi/decimal"
        , SV.version 1 0 0 [] []
        , SV.version 2 0 0 [] []
        , Nothing
        )
    ElmNirum ->
        ( "spoqa/elm-nirum"
        , SV.version 0 1 0 [] []
        , SV.version 0 2 0 [] []
        , Nothing
        )
    ElmUuid ->
        ( "danyx23/elm-uuid"
        , SV.version 2 1 1 [] []
        , SV.version 3 0 0 [] []
        , Nothing
        )

compileElmPackageJson :: Package Elm -> Set ElmLibrary -> Value
compileElmPackageJson
        Package
            { metadata = Metadata { description = description'
                                  , license = license'
                                  , target = target'
                                  , version = version'
                                  }
            , modules = ms
            }
        dependencies' =
    Object
        [ ("version", String $ SV.toText version')
        , ("summary", String $ fromMaybe "" description')
        , ("repository", String $ pack $ show $ repositoryUrl target')
        , ("license", String $ fromMaybe "" license')
        , ("source-directories", Array [String $ pack sourceDirectory])
        , ( "exposed-modules"
          , Array $ fromList $ (String . toImportPath) <$> keys ms
          )
        , ( "dependencies"
          , Object $
                HM.insert "elm-lang/core" (String "5.0.0 <= v < 6.0.0") deps
          )
        , ("dependency-sources", Object sources)
        , ("elm-version", String "0.18.0 <= v < 0.19.0")
        ]
  where
    deps :: Object
    deps = HM.fromList
        [ (pkg, String $ concat [SV.toText lower, " <= v < ", SV.toText upper])
        | (pkg, lower, upper, _) <- toDependency <$> toList dependencies'
        ]
    sources :: Object
    sources = HM.fromList
        [ (pkg, String src)
        | (pkg, _, _, Just src) <- toDependency <$> toList dependencies'
        ]

compilePackage' :: Package Elm -> Map FilePath (Either Error ByteString)
compilePackage' package =
    insert "elm-package.json" (Right $ encodePretty elmPackageJson) $
        fromList
            [ ( filePath
              , modResult >>= \ (_, markup) -> return $ renderMarkup markup
              )
            | (filePath, modResult) <- modResults
            ]
  where
    modResults :: [(FilePath, Either Error (Set ElmLibrary, Markup))]
    modResults =
        [ (toFilePath modulePath', compileModule boundModule)
        | modulePath' <- keys $ modules package
        , Just boundModule <- [resolveBoundModule modulePath' package]
        ]
    libs :: Set ElmLibrary
    libs = unions [deps | (_, Right (deps, _)) <- modResults]
    elmPackageJson :: Value
    elmPackageJson = compileElmPackageJson package libs

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
