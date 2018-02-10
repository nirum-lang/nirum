{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, OverloadedLists,
             QuasiQuotes, TypeFamilies, TypeSynonymInstances,
             MultiParamTypeClasses #-}
module Nirum.Targets.Rust ( Rust
                          , Code
                          , CompileError
                          ) where

import qualified Data.Map.Strict as M
import qualified Data.SemVer as SV
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Typeable (Typeable)

import GHC.Exts (IsList (toList))

import System.FilePath (joinPath, replaceExtension)

import Text.Blaze.Renderer.Text
import Text.Heterocephalus (compileText)

import qualified Nirum.Constructs.Identifier as I
import Nirum.Constructs.Module
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Constructs.Name
import Nirum.Constructs.TypeDeclaration
import Nirum.Package.Metadata
import qualified Nirum.Package.ModuleSet as MS
import Nirum.Targets.Rust.Keyword
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

compileModule :: BoundModule Rust -> Code
compileModule m =
    toStrict $
    renderMarkup [compileText|%{ forall (moduleName, members') <- enums }
pub enum #{ toRustIdentifier I.toPascalCaseText $ facialName moduleName } {
%{ forall EnumMember memberName _ <- members' }
    #{ toRustIdentifier I.toPascalCaseText $ facialName memberName },
%{ endforall }
}
%{ endforall }
|]
  where
    moduleTypes :: [TypeDeclaration]
    moduleTypes = toList $ boundTypes m
    enums :: [(Name, [EnumMember])]
    enums =
        [ (moduleName, toList members')
        | TypeDeclaration { typename = moduleName
                          , type' = EnumType { members = members' }
                          } <- moduleTypes
        ]

compilePackage' :: Package Rust
                -> M.Map FilePath (Either CompileError' Code)
compilePackage' package =
    M.fromList $
        [ ( toFilename mp
          , Right $ compileModule m
          )
        | (mp, _) <- modules'
        , Just m <- [resolveBoundModule mp package]
        ] ++
        [ ("Cargo.toml", Right $ genCargoToml package)
        , (joinPath ["src", "lib.rs"], Right "")
        ]
  where
    convertModulePath :: ModulePath -> [FilePath]
    convertModulePath mp =
        "src" :
        [ T.unpack (toRustIdentifier I.toSnakeCaseText i)
        | i <- toList mp
        ]
    toFilename :: ModulePath -> FilePath
    toFilename mp =
        replaceExtension (joinPath $ convertModulePath mp) "rs"
    modules' :: [(ModulePath, Module)]
    modules' = MS.toAscList $ modules package

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
