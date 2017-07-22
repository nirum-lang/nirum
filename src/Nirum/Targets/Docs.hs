{-# LANGUAGE OverloadedLists, QuasiQuotes, TypeFamilies #-}
module Nirum.Targets.Docs ( Docs
                          , blockToHtml
                          , makeFilePath
                          , makeUri
                          , moduleTitle
                          ) where

import Data.Maybe (mapMaybe)
import GHC.Exts (IsList (fromList, toList))

import Data.ByteString.Lazy (toStrict)
import qualified Text.Email.Parser as E
import Data.Map.Strict (Map, union)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.FilePath ((</>))
import Text.Blaze (ToMarkup (preEscapedToMarkup))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (Html, shamlet)

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Declaration (Documented (docsBlock))
import qualified Nirum.Constructs.Declaration as DE
import qualified Nirum.Constructs.DeclarationSet as DES
import qualified Nirum.Constructs.Docs as D
import Nirum.Constructs.Identifier ( Identifier
                                   , toNormalizedString
                                   , toNormalizedText
                                   )
import Nirum.Constructs.Module (Module (Module, docs))
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Constructs.Name (Name (facialName))
import qualified Nirum.Constructs.Service as S
import qualified Nirum.Constructs.TypeDeclaration as TD
import qualified Nirum.Constructs.TypeExpression as TE
import Nirum.Docs ( Block (Heading)
                  , filterReferences
                  )
import Nirum.Docs.Html (render, renderInlines)
import Nirum.Package ( BoundModule (boundPackage, modulePath)
                     , Package (Package, metadata, modules)
                     , resolveBoundModule
                     , resolveModule
                     , types
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

-- FIXME: remove trailing index.html on production
makeUri :: ModulePath -> T.Text
makeUri modulePath' =
    T.intercalate "/" $
                  map toNormalizedText (toList modulePath') ++ ["index.html"]

layout :: ToMarkup m => Package Docs -> m -> Html -> Html
layout Package { metadata = md } title body = [shamlet|
$doctype 5
<html>
    <head>
        <meta charset="utf-8">
        <title>#{title}
        <meta name="generator" content="Nirum #{versionText}">
        $forall Author { name = name' } <- authors md
            <meta name="author" content="#{name'}">
    <body>#{body}
|]

typeExpression :: BoundModule Docs -> TE.TypeExpression -> Html
typeExpression _ expr = [shamlet|<code>#{typeExpr expr}|]
  where
    typeExpr :: TE.TypeExpression -> Html
    typeExpr expr' = [shamlet|
$case expr'
    $of TE.TypeIdentifier ident
        #{toCode ident}
    $of TE.OptionModifier type'
        #{typeExpr type'}?
    $of TE.SetModifier elementType
        {#{typeExpr elementType}}
    $of TE.ListModifier elementType
        [#{typeExpr elementType}]
    $of TE.MapModifier keyType valueType
        {#{typeExpr keyType}: #{typeExpr valueType}}
|]

module' :: BoundModule Docs -> Html
module' docsModule = layout pkg path $ [shamlet|
    $maybe tit <- title
        <h1><code>#{path}</code> &mdash; #{tit}
    $nothing
        <h1><code>#{path}</code>
    $forall (ident, decl) <- types'
        <div class="#{showKind decl}" id="#{toNormalizedText ident}">
            #{typeDecl docsModule ident decl}
|]
  where
    pkg :: Package Docs
    pkg = boundPackage docsModule
    path :: T.Text
    path = toCode $ modulePath docsModule
    types' :: [(Identifier, TD.TypeDeclaration)]
    types' = [ (facialName $ DE.name decl, decl)
             | decl <- DES.toList $ types docsModule
             , case decl of
                    TD.Import {} -> False
                    _ -> True
             ]
    mod' :: Maybe Module
    mod' = resolveModule (modulePath docsModule) pkg
    title :: Maybe Html
    title = do
        m <- mod'
        moduleTitle m

blockToHtml :: Block -> Html
blockToHtml b = preEscapedToMarkup $ render b

typeDecl :: BoundModule Docs -> Identifier -> TD.TypeDeclaration -> Html
typeDecl mod' ident
         tc@TD.TypeDeclaration { TD.type' = TD.Alias cname } = [shamlet|
    <h2>type <code>#{toNormalizedText ident}</code>
    $maybe d <- docsBlock tc
        <p>#{blockToHtml d}
    <p>= <span class="canonical-type">#{typeExpression mod' cname}</span>
|]
typeDecl mod' ident
         tc@TD.TypeDeclaration { TD.type' = TD.UnboxedType innerType } =
    [shamlet|
        <h2>unboxed <code>#{toNormalizedText ident}</code>
        $maybe d <- docsBlock tc
            <p>#{blockToHtml d}
        <p>(<span class="inner-type">#{typeExpression mod' innerType}</span>)
    |]
typeDecl _ ident
         tc@TD.TypeDeclaration { TD.type' = TD.EnumType members } = [shamlet|
    <h2>enum <code>#{toNormalizedText ident}</code>
    $maybe d <- docsBlock tc
        <p>#{blockToHtml d}
    <dl class="members">
        $forall decl <- DES.toList members
            <dt class="member-name"><code>#{nameText $ DE.name decl}</code>
                <dd class="member-doc">
                    $maybe d <- docsBlock decl
                        #{blockToHtml d}
|]
typeDecl mod' ident
         tc@TD.TypeDeclaration { TD.type' = TD.RecordType fields } = [shamlet|
    <h2>record <code>#{toNormalizedText ident}</code>
    $maybe d <- docsBlock tc
        <p>#{blockToHtml d}
    <dl class="fields">
        $forall fieldDecl@(TD.Field _ fieldType _) <- DES.toList fields
            <dt class="field-name"><code>#{nameText $ DE.name fieldDecl}</code>
            <dd class="field-type">#{typeExpression mod' fieldType}
            $maybe d <- docsBlock fieldDecl
                <dd>#{blockToHtml d}
|]
typeDecl mod' ident
         tc@TD.TypeDeclaration { TD.type' = TD.UnionType tags } = [shamlet|
    <h2>union <code>#{toNormalizedText ident}</code>
    $maybe d <- docsBlock tc
        <p>#{blockToHtml d}
    $forall tagDecl@(TD.Tag _ fields _) <- DES.toList tags
        <h3 class="tag">
            <code>#{nameText $ DE.name tagDecl}
        $maybe d <- docsBlock tagDecl
            <p>#{blockToHtml d}
        <dl class="fields">
            $forall fieldDecl@(TD.Field _ fieldType _) <- DES.toList fields
                <dt class="field-name">
                    <code>#{nameText $ DE.name fieldDecl}
                <dd class="field-type">#{typeExpression mod' fieldType}
                $maybe d <- docsBlock fieldDecl
                    <dd>#{blockToHtml d}
|]
typeDecl _ ident
         TD.TypeDeclaration { TD.type' = TD.PrimitiveType {} } = [shamlet|
    <h2>primitive <code>#{toNormalizedText ident}</code>
|]
typeDecl mod' ident
         tc@TD.ServiceDeclaration { TD.service = S.Service methods } =
    [shamlet|
        <h2>service <code>#{toNormalizedText ident}</code>
        $maybe d <- docsBlock tc
            <p>#{blockToHtml d}
        $forall methodDecl@(S.Method _ ps ret err _) <- DES.toList methods
            <h3 class="method">
                <code class="method-name">#{nameText $ DE.name methodDecl}()
                &rarr;
                <code class="return-type">#{typeExpression mod' ret}
            $maybe d <- docsBlock methodDecl
                <p>#{blockToHtml d}
            $maybe errType <- err
                <p class="error-type">#{typeExpression mod' errType}
            <dl class="parameters">
                $forall paramDecl@(S.Parameter _ paramType _) <- DES.toList ps
                    <dt class="parameter-name">
                        <code>#{nameText $ DE.name paramDecl}
                    <dd class="parameter-type">#{typeExpression mod' paramType}
                    $maybe d <- docsBlock paramDecl
                        <dd>#{blockToHtml d}
|]
typeDecl _ _ TD.Import {} =
    error ("It shouldn't happen; please report it to Nirum's bug tracker:\n" ++
           "https://github.com/spoqa/nirum/issues")

nameText :: Name -> T.Text
nameText = toNormalizedText . facialName

showKind :: TD.TypeDeclaration -> T.Text
showKind TD.ServiceDeclaration {} = "service"
showKind TD.TypeDeclaration { TD.type' = type'' } = case type'' of
    TD.Alias {} -> "alias"
    TD.UnboxedType {} -> "unboxed"
    TD.EnumType {} -> "enum"
    TD.RecordType {} -> "record"
    TD.UnionType {} -> "union"
    TD.PrimitiveType {} -> "primitive"
showKind TD.Import {} = "import"

contents :: Package Docs -> Html
contents pkg@Package { metadata = md
                     , modules = ms
                     } = layout pkg ("Package docs" :: T.Text) [shamlet|
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
    emailText :: E.EmailAddress -> T.Text
    emailText = decodeUtf8 . E.toByteString

moduleTitle :: Module -> Maybe Html
moduleTitle Module { docs = docs' } = do
    d <- docs'
    t <- D.title d
    nodes <- case t of
                 Heading _ inlines ->
                    Just $ filterReferences inlines
                 _ -> Nothing
    return $ preEscapedToMarkup $ renderInlines nodes

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
