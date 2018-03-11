{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, OverloadedLists,
             QuasiQuotes, TypeFamilies, TypeSynonymInstances,
             MultiParamTypeClasses #-}
module Nirum.Targets.Rust ( Rust
                          , Code
                          , CompileError
                          , childModules
                          ) where

import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.SemVer as SV
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Typeable (Typeable)
import System.FilePath

import GHC.Exts (IsList (toList))

import Text.Blaze.Renderer.Text
import Text.Heterocephalus (compileText)

import qualified Nirum.Constructs.Identifier as I
import Nirum.Constructs.ModulePath
import Nirum.Constructs.TypeDeclaration
import Nirum.Package.Metadata
import qualified Nirum.Package.ModuleSet as MS
import Nirum.Targets.Rust.Item
import Nirum.Targets.Rust.Keyword
import Nirum.Targets.Rust.ModuleTree
import Nirum.TypeInstance.BoundModule

data Rust = Rust { packageName :: T.Text
                 }
            deriving (Eq, Ord, Show, Typeable)

type Code = T.Text
type CompileError' = ()

genCargoToml :: Package Rust -> Code
genCargoToml Package { metadata = Metadata { version = version'
                                           , target = Rust { packageName = name' }
                                           }
                     } =
    toStrict $
    renderMarkup [compileText|[package]
name = "#{ name' }"
version = "#{ SV.toLazyText version' }"
|]

buildPrologue :: RustModule -> TL.Text
buildPrologue mod' =
    renderMarkup [compileText|%{ forall child <- children mod' }
pub mod #{ toRustIdentifier I.toCamelCaseText child };
%{ endforall }
|]

buildBody :: Maybe (BoundModule Rust) -> TL.Text
buildBody (Just m) =
    TL.concat [renderItem i | i <- moduleTypes]
  where
    moduleTypes :: [TypeDeclaration]
    moduleTypes = toList $ boundTypes m
buildBody Nothing = TL.empty

compilePackage' :: Package Rust
                -> M.Map FilePath (Either CompileError' Code)
compilePackage' package =
    M.fromList $
        [ ( fileName
          , Right $
            toStrict $
            TL.append (buildPrologue mod')
                      (buildBody (resolveWithModulePath mp))
          )
        | mod'@RustModule { filePath = fileName
                          , modPath = mp
                          } <- modules'
        ] ++
        [ ("Cargo.toml", Right $ genCargoToml package) ]
  where
    resolveWithModulePath :: ModulePath -> Maybe (BoundModule Rust)
    resolveWithModulePath mp = resolveBoundModule mp package
    modules' :: [RustModule]
    modules' = libModule : [ RustModule { filePath = toFilePath $ toList mp
                                        , modPath = mp
                                        , children = childModules expanded' mp
                                        }
                           | mp <- toList expanded' ]
    expanded' = hierarchies $ S.fromList $ MS.keys $ modules package
    libModule = RustModule { filePath = toFilePath []
                           , modPath = ["lib"]
                           , children = S.map root expanded'
                           }

childModules :: Foldable f => f ModulePath -> ModulePath -> S.Set I.Identifier
childModules modPaths base = fromMaybe mempty $ getOption $ foldMap f modPaths
  where
    f = Option . fmap (S.singleton . root) . stripPrefix base

toFilePath :: [I.Identifier] -> FilePath
toFilePath [] = joinPath ["src", "lib.rs"]
toFilePath p = joinPath (["src"] ++ convert p ++ ["mod.rs"])
  where
    convert = map (T.unpack . toRustIdentifier I.toSnakeCaseText)

instance Target Rust where
    type CompileResult Rust = Code
    type CompileError Rust = CompileError'

    targetName _ = "rust"
    parseTarget table = do
        name' <- stringField "name" table
        return Rust { packageName = name'
                    }
    compilePackage = compilePackage'
    showCompileError _ _ = ""
    toByteString _ = encodeUtf8
