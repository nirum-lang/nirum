{-# LANGUAGE OverloadedLists, OverloadedStrings, QuasiQuotes, TypeFamilies #-}
module Nirum.Targets.Docs (Docs) where

import Data.ByteString.Lazy (toStrict)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (Html, shamlet)

import Nirum.Constructs (Construct (toCode))
import Nirum.Package (Package (Package, metadata, modules))
import Nirum.Package.Metadata ( Author (Author, name)
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

type Error = Text

index :: Package Docs -> Html
index Package { metadata = md, modules = ms } = [shamlet|
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
            $forall (modulePath, _) <- MS.toAscList ms
                <li><code>#{toCode modulePath}</code>
|]

compilePackage' :: Package Docs -> Map FilePath (Either Error Html)
compilePackage' pkg =
    [("index.html", Right $ index pkg)]

instance Target Docs where
    type CompileResult Docs = Html
    type CompileError Docs = Error
    targetName _ = "docs"
    parseTarget _ = return Docs
    compilePackage = compilePackage'
    showCompileError _ = id
    toByteString _ = toStrict . renderHtml
