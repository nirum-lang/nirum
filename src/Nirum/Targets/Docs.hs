{-# LANGUAGE OverloadedLists, QuasiQuotes, TypeFamilies #-}
module Nirum.Targets.Docs (Docs, makeFilePath, makeUri) where

import Data.Maybe (mapMaybe)
import GHC.Exts (IsList (fromList, toList))

import Data.ByteString.Lazy (toStrict)
import qualified Text.Email.Parser as E
import Data.Map.Strict (Map, union)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.FilePath ((</>))
import Text.Blaze (preEscapedToMarkup)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (Html, shamlet)

import Nirum.Constructs (Construct (toCode))
import qualified Nirum.Constructs.Docs as D
import Nirum.Constructs.Identifier (toNormalizedString)
import Nirum.Constructs.Module (Module (Module, docs))
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Docs ( Block (Heading)
                  , filterReferences
                  )
import Nirum.Docs.Html (renderInlines)
import Nirum.Package ( BoundModule (boundPackage, modulePath)
                     , Package (Package, metadata, modules)
                     , resolveBoundModule
                     )
import Nirum.Package.Metadata ( Author (Author, email, name, uri)
                              , Metadata (authors)
                              , Target ( CompileError
                                       , CompileResult
                                       , compilePackage
                                       , parseTarget
                                       , showCompileError
                                       , targetName
                                       , toByteString
                                       )
                              )
import qualified Nirum.Package.ModuleSet as MS
import Nirum.Version (versionText)

data Docs = Docs deriving (Eq, Ord, Show)

type Error = T.Text

makeFilePath :: ModulePath -> FilePath
makeFilePath modulePath' = foldl (</>) "" $
    map toNormalizedString (toList modulePath') ++ ["index.html"]

makeUri :: ModulePath -> String
makeUri modulePath' = "./" ++ (makeFilePath modulePath')

module' :: BoundModule Docs -> Html
module' docsModule = [shamlet|
$doctype 5
<html>
    <head>
        <meta charset="utf-8">
        <title>#{path}
        <meta name="generator" content="Nirum #{versionText}">
        $forall Author { name = name' } <- authors md
            <meta name="author" content="#{name'}">
    <body>
        <h1>
            <code>#{path}
|]
  where
    md :: Metadata Docs
    md = metadata $ boundPackage docsModule
    path :: T.Text
    path = toCode $ modulePath docsModule

contents :: Package Docs -> Html
contents Package { metadata = md, modules = ms } = [shamlet|
$doctype 5
<html>
    <head>
        <meta charset="utf-8">
        <title>Package docs
        <meta name="generator" content="Nirum #{versionText}">
        $forall Author { name = name' } <- authors md
            <meta name="author" content="#{name'}">
    <body>
        <h1>Modules
        <ul>
            $forall (modulePath', mod) <- MS.toAscList ms
                <li>
                    <a href="#{makeUri modulePath'}">
                        <code>#{toCode modulePath'} </code>
                            $maybe tit <- moduleTitle mod
                                &mdash; #{tit}
        <hr>
        <dl>
            <dt.author>
                $if 1 < length (authors md)
                    Authors
                $else
                    Author
            $forall Author { name = n, uri = u, email = e } <- authors md
                $maybe uri' <- u
                    <dd.author><a href="#{show uri'}">#{n}</a>
                $nothing
                    $maybe email' <- e
                        <dd.author><a href="mailto:#{emailText email'}">#{n}</a>
                    $nothing
                        <dd.author>#{n}
|]
  where
    moduleTitle :: Module -> Maybe Html
    moduleTitle Module { docs = docs' } = do
        d <- docs'
        t <- D.title d
        nodes <- case t of
                     Heading _ inlines ->
                        Just $ filterReferences inlines
                     _ -> Nothing
        return $ preEscapedToMarkup $ renderInlines nodes
    emailText :: E.EmailAddress -> T.Text
    emailText = decodeUtf8 . E.toByteString

compilePackage' :: Package Docs -> Map FilePath (Either Error Html)
compilePackage' pkg =
    [("index.html", Right $ contents pkg)] `union`
          (fromList [ (makeFilePath $ modulePath m, Right $ module' m)
                    | m <- modules'
                    ] :: Map FilePath (Either Error Html))
  where
    paths' :: [ModulePath]
    paths' = MS.keys $ modules pkg
    modules' :: [BoundModule Docs]
    modules' = mapMaybe (`resolveBoundModule` pkg) paths'

instance Target Docs where
    type CompileResult Docs = Html
    type CompileError Docs = Error
    targetName _ = "docs"
    parseTarget _ = return Docs
    compilePackage = compilePackage'
    showCompileError _ = id
    toByteString _ = toStrict . renderHtml
