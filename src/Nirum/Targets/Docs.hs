{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
module Nirum.Targets.Docs ( Docs
                          , blockToHtml
                          , makeFilePath
                          , makeUri
                          , moduleTitle
                          ) where

import Data.Maybe (mapMaybe)
import GHC.Exts (IsList (fromList, toList))

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import qualified Text.Email.Parser as E
import Data.Map.Strict (Map, union)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.FilePath ((</>))
import Text.Blaze (ToMarkup (preEscapedToMarkup))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Cassius (cassius, renderCss)
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

layout :: ToMarkup m => Package Docs -> Int -> m -> Html -> Html
layout Package { metadata = md } dirDepth title body = [shamlet|
$doctype 5
<html>
    <head>
        <meta charset="utf-8">
        <title>#{title}
        <meta name="generator" content="Nirum #{versionText}">
        $forall Author { name = name' } <- authors md
            <meta name="author" content="#{name'}">
        <link rel="stylesheet" href="#{T.replicate dirDepth "../"}style.css">
    <body>#{body}
|]

typeExpression :: BoundModule Docs -> TE.TypeExpression -> Html
typeExpression _ expr = [shamlet|#{typeExpr expr}|]
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
module' docsModule = layout pkg depth path $ [shamlet|
    $maybe tit <- title
        <h1><code>#{path}</code>
        <p>#{tit}
    $nothing
        <h1><code>#{path}</code>
    $forall (ident, decl) <- types'
        <div class="#{showKind decl}" id="#{toNormalizedText ident}">
            #{typeDecl docsModule ident decl}
|]
  where
    docsModulePath :: ModulePath
    docsModulePath = modulePath docsModule
    pkg :: Package Docs
    pkg = boundPackage docsModule
    path :: T.Text
    path = toCode docsModulePath
    types' :: [(Identifier, TD.TypeDeclaration)]
    types' = [ (facialName $ DE.name decl, decl)
             | decl <- DES.toList $ types docsModule
             , case decl of
                    TD.Import {} -> False
                    _ -> True
             ]
    mod' :: Maybe Module
    mod' = resolveModule docsModulePath pkg
    title :: Maybe Html
    title = do
        m <- mod'
        moduleTitle m
    depth :: Int
    depth = length $ toList docsModulePath

blockToHtml :: Block -> Html
blockToHtml b = preEscapedToMarkup $ render b

typeDecl :: BoundModule Docs -> Identifier -> TD.TypeDeclaration -> Html
typeDecl mod' ident
         tc@TD.TypeDeclaration { TD.type' = TD.Alias cname } = [shamlet|
    <h2>
        <span.type>type
        <code>#{toNormalizedText ident}</code>
        =
        <code>#{typeExpression mod' cname}</code>
    $maybe d <- docsBlock tc
        #{blockToHtml d}
|]
typeDecl mod' ident
         tc@TD.TypeDeclaration { TD.type' = TD.UnboxedType innerType } =
    [shamlet|
        <h2>
            <span.type>unboxed
            <code>#{toNormalizedText ident} (#{typeExpression mod' innerType})
        $maybe d <- docsBlock tc
            #{blockToHtml d}
    |]
typeDecl _ ident
         tc@TD.TypeDeclaration { TD.type' = TD.EnumType members } = [shamlet|
    <h2>
        <span.type>enum
        <code>#{toNormalizedText ident}
    $maybe d <- docsBlock tc
        #{blockToHtml d}
    <dl class="members">
        $forall decl <- DES.toList members
            <dt class="member-name"><code>#{nameText $ DE.name decl}</code>
                <dd class="member-doc">
                    $maybe d <- docsBlock decl
                        #{blockToHtml d}
|]
typeDecl mod' ident
         tc@TD.TypeDeclaration { TD.type' = TD.RecordType fields } = [shamlet|
    <h2>
        <span.type>record
        <code>#{toNormalizedText ident}
    $maybe d <- docsBlock tc
        #{blockToHtml d}
    $forall fieldDecl@(TD.Field _ fieldType _) <- DES.toList fields
        <h3>
            <span.type>#{typeExpression mod' fieldType}
            <code>#{nameText $ DE.name fieldDecl}
        $maybe d <- docsBlock fieldDecl
            #{blockToHtml d}
|]
typeDecl mod' ident
         tc@TD.TypeDeclaration { TD.type' = TD.UnionType tags } = [shamlet|
    <h2>union <code>#{toNormalizedText ident}</code>
    $maybe d <- docsBlock tc
        #{blockToHtml d}
    $forall tagDecl@(TD.Tag _ fields _) <- DES.toList tags
        <h3 class="tag"><code>#{nameText $ DE.name tagDecl}</code>
        $maybe d <- docsBlock tagDecl
            #{blockToHtml d}
        $forall fieldDecl@(TD.Field _ fieldType _) <- DES.toList fields
            <h4>
                <span.type>#{typeExpression mod' fieldType}
                <code>#{nameText $ DE.name fieldDecl}
            $maybe d <- docsBlock fieldDecl
                #{blockToHtml d}
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
            #{blockToHtml d}
        $forall md@(S.Method _ ps ret err _) <- DES.toList methods
            <h3 class="method">
                <code class="method-name">#{nameText $ DE.name md}</code>(
                    <i>
                        $forall pd@(S.Parameter _ pt _) <- DES.toList ps
                            #{typeExpression mod' pt} #{nameText $ DE.name pd}
                    )
            $maybe d <- docsBlock md
                #{blockToHtml d}
            <dl class="result">
                <dt class="return-label">returns:
                <dd class="return-type">#{typeExpression mod' ret}
                $maybe errType <- err
                    <dt class="raise-label">raises:
                    <dd class="raise-type">#{typeExpression mod' errType}
            $forall paramDecl@(S.Parameter _ paramType _) <- DES.toList ps
                $maybe d <- docsBlock paramDecl
                    <h4>
                        <span.type>#{typeExpression mod' paramType}
                        <code>#{nameText $ DE.name paramDecl}</code>:
                    #{blockToHtml d}
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
                     } = layout pkg 0 ("Package docs" :: T.Text) [shamlet|
<h1>Modules
$forall (modulePath', mod) <- MS.toAscList ms
    $maybe tit <- moduleTitle mod
        <h2>
            <a href="#{makeUri modulePath'}"><code>#{toCode modulePath'}</code>
        <p>#{tit}
    $nothing
        <h2>
            <a href="#{makeUri modulePath'}"><code>#{toCode modulePath'}</code>
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

stylesheet :: TL.Text
stylesheet = renderCss ([cassius|
@import url(
https://fonts.googleapis.com/css?family=Source+Code+Pro:300,400|Source+Sans+Pro
)
body
    font-family: Source Sans Pro
    color: #{gray8}
code
    font-family: Source Code Pro
    font-weight: 300
    background-color: #{gray1}
pre
    padding: 16px 10px
    background-color: #{gray1}
    code
        background: none
div
    border-top: 1px solid #{gray3}
h1, h2, h3, h4, h5, h6
    code
        font-weight: 400
        background-color: #{gray3}
a
    text-decoration: none
a:link
    color: #{indigo8}
a:visited
    color: #{graph8}
a:hover
    text-decoration: underline
|] undefined)
  where
    -- from Open Color https://yeun.github.io/open-color/
    gray1 :: T.Text
    gray1 = "#f1f3f5"
    gray3 :: T.Text
    gray3 = "#dee2e6"
    gray8 :: T.Text
    gray8 = "#343a40"
    graph8 :: T.Text
    graph8 = "#9c36b5"
    indigo8 :: T.Text
    indigo8 = "#3b5bdb"

compilePackage' :: Package Docs -> Map FilePath (Either Error BS.ByteString)
compilePackage' pkg =
    fromList [ ("style.css", Right $ encodeUtf8 $ TL.toStrict stylesheet)
             , ("index.html", Right $ toStrict $ renderHtml $ contents pkg)
             ] `union`
          (fromList [ ( makeFilePath $ modulePath m
                      , Right $ toStrict $ renderHtml $ module' m
                      )
                    | m <- modules'
                    ] :: Map FilePath (Either Error BS.ByteString))
  where
    paths' :: [ModulePath]
    paths' = MS.keys $ modules pkg
    modules' :: [BoundModule Docs]
    modules' = mapMaybe (`resolveBoundModule` pkg) paths'

instance Target Docs where
    type CompileResult Docs = BS.ByteString
    type CompileError Docs = Error
    targetName _ = "docs"
    parseTarget _ = return Docs
    compilePackage = compilePackage'
    showCompileError _ = id
    toByteString _ = id
