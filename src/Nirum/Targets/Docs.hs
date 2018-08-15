{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
module Nirum.Targets.Docs ( Docs (..)
                          , blockToHtml
                          , makeFilePath
                          , makeUri
                          , moduleTitle
                          ) where

import Data.Char
import qualified Data.List
import Data.Maybe
import GHC.Exts (IsList (fromList, toList))

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import qualified Text.Email.Parser as E
import Data.Map.Strict (Map, mapKeys, mapWithKey, unions)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.FilePath
import Text.Blaze (ToMarkup (preEscapedToMarkup))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Cassius
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
import Nirum.Docs ( Block (..)
                  , Inline (..)
                  , extractTitle
                  , filterReferences
                  , transformReferences
                  , trimTitle
                  )
import Nirum.Docs.Html (render, renderInlines, renderLinklessInlines)
import Nirum.Package
import Nirum.Package.Metadata hiding (target)
import qualified Nirum.Package.ModuleSet as MS
import Nirum.TypeInstance.BoundModule
import Nirum.Version (versionText)

data Docs = Docs
    { docsTitle :: T.Text
    , docsStyle :: T.Text
    , docsHeader :: T.Text
    , docsFooter :: T.Text
    } deriving (Eq, Ord, Show)

type Error = T.Text

data CurrentPage
    = IndexPage
    | ModulePage ModulePath
    | DocumentPage FilePath
    deriving (Eq, Show)

makeFilePath :: ModulePath -> FilePath
makeFilePath modulePath' = foldl (</>) "" $
    map toNormalizedString (toList modulePath') ++ ["index.html"]

-- FIXME: remove trailing index.html on production
makeUri :: ModulePath -> T.Text
makeUri modulePath' =
    T.intercalate "/" $
                  map toNormalizedText (toList modulePath') ++ ["index.html"]

layout :: Package Docs -> Int -> CurrentPage -> T.Text -> Html -> Html
layout pkg dirDepth currentPage title body =
    layout' pkg dirDepth currentPage title body Nothing

layout' :: Package Docs
        -> Int
        -> CurrentPage
        -> T.Text
        -> Html
        -> Maybe Html
        -> Html
layout' pkg@Package { metadata = md, modules = ms }
        dirDepth currentPage title body footer = [shamlet|
$doctype 5
<html>
    <head>
        <meta charset="utf-8">
        <title>
            $if (title == docsTitle (target pkg))
                #{title}
            $else
                #{title} &mdash; #{docsTitle (target pkg)}
        <meta name="generator" content="Nirum #{versionText}">
        $forall Author { name = name' } <- authors md
            <meta name="author" content="#{name'}">
        <link rel="stylesheet" href="#{root}style.css">
    <body>
        #{preEscapedToMarkup $ docsHeader $ target pkg}
        <nav>
            $if currentPage == IndexPage
                <a class="index selected" href="#{root}index.html">
                    <strong>
                        #{docsTitle $ target pkg}
            $else
                <a class="index" href="#{root}index.html">
                    #{docsTitle $ target pkg}
            <ul.manuals.toc>
                $forall (documentPath, doc) <- documentPairs
                    $if currentPage == DocumentPage documentPath
                        <li.selected>
                            <a href="#{root}#{documentHtmlPath documentPath}">
                                <strong>
                                    #{renderDocumentTitle documentPath doc}
                    $else
                        <li>
                            <a href="#{root}#{documentHtmlPath documentPath}">
                                #{renderDocumentTitle documentPath doc}
            <ul.modules.toc>
                $forall (modulePath', mod) <- modulePairs
                    $if currentPage == ModulePage modulePath'
                        <li.selected>
                            <a href="#{root}#{makeUri modulePath'}">
                                <strong>
                                    <code>#{toCode modulePath'}</code>
                                    $maybe tit <- moduleTitle mod
                                        &mdash; #{tit}
                    $else
                        <li>
                            <a href="#{root}#{makeUri modulePath'}">
                                <code>#{toCode modulePath'}</code>
                                $maybe tit <- moduleTitle mod
                                    &mdash; #{tit}
        <article>#{body}
        $maybe f <- footer
            <footer>#{f}
        #{preEscapedToMarkup $ docsFooter $ target pkg}
|]
  where
    root :: T.Text
    root = T.replicate dirDepth "../"
    modulePairs :: [(ModulePath, Module)]
    modulePairs = MS.toAscList ms
    documentPairs :: [(FilePath, D.Docs)]
    documentPairs = Data.List.sortOn
        documentSortKey
        (toList $ fst $ listDocuments pkg)
    documentSortKey :: (FilePath, D.Docs) -> (Bool, Int, FilePath)
    documentSortKey ("", _) = (False, 0, "")
    documentSortKey (fp@(fp1 : _), _) =
        (isUpper fp1, length (filter (== pathSeparator) fp), fp)

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
module' docsModule =
    layout pkg depth (ModulePage docsModulePath) path [shamlet|
$maybe tit <- headingTitle
    <h1>#{path}
    <p>#{tit}
$nothing
    <h1>#{path}
$maybe m <- mod'
    $maybe d <- docsBlock m
        #{blockToHtml (trimTitle d)}
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
             | decl <- DES.toList $ boundTypes docsModule
             , case decl of
                    TD.Import {} -> False
                    _ -> True
             ]
    mod' :: Maybe Module
    mod' = resolveModule docsModulePath pkg
    headingTitle :: Maybe Html
    headingTitle = do
        m <- mod'
        moduleTitle m
    depth :: Int
    depth = length $ toList docsModulePath

blockToHtml :: Block -> Html
blockToHtml =
    preEscapedToMarkup . render . transformReferences replaceUrl
  where
    replaceUrl :: T.Text -> T.Text
    replaceUrl url
      | isAbsoluteUrl url = url
      | otherwise =
            let (path, frag) = T.break (== '#') url
            in T.pack (documentHtmlPath $ T.unpack path) `T.append` frag
    isAbsoluteUrl :: T.Text -> Bool
    isAbsoluteUrl url
      | T.null rest = False
      | T.null scheme = True
      | otherwise = T.all testChar scheme && isAlpha (T.head scheme)
      where
        (scheme, rest) = T.break (== ':') url
    testChar :: Char -> Bool
    testChar c = (isAscii c && isAlphaNum c) || c == '.' || c == '-'

typeDecl :: BoundModule Docs -> Identifier -> TD.TypeDeclaration -> Html
typeDecl mod' ident
         tc@TD.TypeDeclaration { TD.type' = TD.Alias cname } = [shamlet|
    <h2 id="#{toNormalizedText ident}">
        <code>type</code> <dfn>#{toNormalizedText ident}</dfn> = #
        <code.type>#{typeExpression mod' cname}</code>
        <a class="anchor" href="##{toNormalizedText ident}">¶
    $maybe d <- docsBlock tc
        #{blockToHtml d}
|]
typeDecl mod' ident
         tc@TD.TypeDeclaration { TD.type' = TD.UnboxedType innerType } =
    [shamlet|
        <h2 id="#{toNormalizedText ident}">
            <code>unboxed</code> <dfn>#{toNormalizedText ident}</dfn>
            (<code>#{typeExpression mod' innerType}</code>)
            <a class="anchor" href="##{toNormalizedText ident}">¶
        $maybe d <- docsBlock tc
            #{blockToHtml d}
    |]
typeDecl _ ident
         tc@TD.TypeDeclaration { TD.type' = TD.EnumType members } = [shamlet|
    <h2 id="#{toNormalizedText ident}">
        <code>enum</code> <dfn>#{toNormalizedText ident}</dfn>
        <a class="anchor" href="##{toNormalizedText ident}">¶
    $maybe d <- docsBlock tc
        #{blockToHtml d}
    <dl class="members">
        $forall decl <- DES.toList members
            <dt class="member-name">
                #{nameText $ DE.name decl}
            <dd class="member-doc">
                $maybe d <- docsBlock decl
                    #{blockToHtml d}
|]
typeDecl mod' ident
         tc@TD.TypeDeclaration { TD.type' = TD.RecordType fields } = [shamlet|
    <h2 id="#{toNormalizedText ident}">
        <code>record</code> <dfn>#{toNormalizedText ident}</dfn>
        <a class="anchor" href="##{toNormalizedText ident}">¶
    $maybe d <- docsBlock tc
        #{blockToHtml d}
    <dl.fields>
        $forall fieldDecl@(TD.Field _ fieldType _) <- DES.toList fields
            <dt id="#{anchorText ident fieldDecl}">
                <code.type>#{typeExpression mod' fieldType}
                <var>#{nameText $ DE.name fieldDecl}
                <a class="anchor" href="##{anchorText ident fieldDecl}">¶
            $maybe d <- docsBlock fieldDecl
                <dd>#{blockToHtml d}
|]
typeDecl mod' ident
         tc@TD.TypeDeclaration
             { TD.type' = unionType@TD.UnionType
                   { TD.defaultTag = defaultTag
                   }
             } =
    [shamlet|
    <h2 id="#{toNormalizedText ident}">
        <code>union</code> <dfn>#{toNormalizedText ident}</dfn>
        <a class="anchor" href="##{toNormalizedText ident}">¶
    $maybe d <- docsBlock tc
        #{blockToHtml d}
    $forall (default_, tagDecl@(TD.Tag _ fields _)) <- tagList
        <h3 .tag :default_:.default-tag id="#{anchorText ident tagDecl}">
            <code>
                $if default_
                    default tag
                $else
                    tag
            <dfn>#{nameText $ DE.name tagDecl}
            <a class="anchor" href="##{anchorText ident tagDecl}">¶
        $maybe d <- docsBlock tagDecl
            #{blockToHtml d}
        <dl.fields>
            $forall fieldDecl@(TD.Field _ fieldType _) <- DES.toList fields
                <dt id="#{anchorText ident fieldDecl}">
                    <code.type>#{typeExpression mod' fieldType}
                    <var>#{nameText $ DE.name fieldDecl}
                    <a class="anchor" href="##{anchorText ident fieldDecl}">¶
                $maybe d <- docsBlock fieldDecl
                    <dd>#{blockToHtml d}
    |]
  where
    tagList :: [(Bool, TD.Tag)]
    tagList =
        [ (defaultTag == Just tag, tag)
        | tag <- DES.toList (TD.tags unionType)
        ]
typeDecl _ ident
         TD.TypeDeclaration { TD.type' = TD.PrimitiveType {} } = [shamlet|
    <h2 id="#{toNormalizedText ident}">
        primitive <code>#{toNormalizedText ident}</code>
        <a class="anchor" href="##{toNormalizedText ident}">¶
|]
typeDecl mod' ident
         tc@TD.ServiceDeclaration { TD.service = S.Service methods } =
    [shamlet|
        <h2 id="#{toNormalizedText ident}">
            <code>service</code>
            <dfn>#{toNormalizedText ident}</dfn>
            <a class="anchor" href="##{toNormalizedText ident}">¶
        $maybe d <- docsBlock tc
            #{blockToHtml d}
        $forall md@(S.Method _ ps ret err _) <- DES.toList methods
            <h3.method>
                $maybe retType <- ret
                    <span.return>
                        <code.type>#{typeExpression mod' retType}</code> #
                <dfn>#{nameText $ DE.name md}</dfn> <span.parentheses>()</span>
                $maybe errType <- err
                    <span.error>
                        throws #
                        <code.error.type>#{typeExpression mod' errType}
            $maybe d <- docsBlock md
                #{blockToHtml d}
            <dl.parameters>
                $forall paramDecl@(S.Parameter _ paramType _) <- DES.toList ps
                    $maybe d <- docsBlock paramDecl
                        <dt>
                            <code.type>#{typeExpression mod' paramType}
                            <code><var>#{nameText $ DE.name paramDecl}</var>
                        <dd>#{blockToHtml d}
|]
typeDecl _ _ TD.Import {} =
    error ("It shouldn't happen; please report it to Nirum's bug tracker:\n" ++
           "https://github.com/nirum-lang/nirum/issues")

nameText :: Name -> T.Text
nameText = toNormalizedText . facialName

-- FIXME: TD.Field 대신 TD.Tag도 받을 수 있도록 해야 함
anchorText :: Identifier -> TD.Field -> T.Text
anchorText i d = T.concat [ toNormalizedText i
                          , "."
                          , nameText $ DE.name d
                          ]

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

readmePage :: Package Docs -> D.Docs -> Html
readmePage pkg docs' =
    layout pkg 0 IndexPage (docsTitle $ target pkg) content
  where
    content :: Html
    content = blockToHtml $ D.toBlock docs'

documentPage :: Package Docs -> FilePath -> D.Docs -> Html
documentPage pkg filePath docs' =
    layout pkg depth (DocumentPage filePath) title' content
  where
    title' :: T.Text
    title' = documentTitleText filePath docs'
    content :: Html
    content = blockToHtml $ D.toBlock docs'
    depth :: Int
    depth = length (splitPath (documentHtmlPath filePath)) - 1

documentHtmlPath :: FilePath -> FilePath
documentHtmlPath = (-<.> "html")

documentTitle :: FilePath -> D.Docs -> [Inline]
documentTitle filePath document =
    case extractTitle $ D.toBlock document of
        Just (_, inlines) -> inlines
        Nothing -> [Text $ T.pack filePath]

renderDocumentTitle :: FilePath -> D.Docs -> Html
renderDocumentTitle filePath =
    preEscapedToMarkup . renderLinklessInlines . documentTitle filePath

documentTitleText :: FilePath -> D.Docs -> T.Text
documentTitleText filePath =
    renderInlines' . documentTitle filePath
  where
    renderInline :: Inline -> T.Text
    renderInline (Text t) = t
    renderInline SoftLineBreak = "\n"
    renderInline HardLineBreak = "\n"
    renderInline (HtmlInline _) = ""
    renderInline (Code code') = code'
    renderInline (Emphasis inlines) = renderInlines inlines
    renderInline (Strong inlines) = renderInlines inlines
    renderInline (Link _ _ inlines) = renderInlines inlines
    renderInline (Image _ title)
      | T.null title = ""
      | otherwise = title
    renderInlines' :: [Inline] -> T.Text
    renderInlines' = T.concat . fmap renderInline

contents :: Package Docs -> Html
contents pkg@Package { metadata = md
                     , modules = ms
                     } =
    layout' pkg 0 IndexPage title body $ case authors md of
        [] -> Nothing
        _ : _ -> Just footer
  where
    body :: Html
    body = [shamlet|
<h1>Modules
$forall (modulePath', mod) <- MS.toAscList ms
    <h2>
        <a href="#{makeUri modulePath'}">
            <code>#{toCode modulePath'}</code>
            $maybe tit <- moduleTitle mod
                &mdash; #{tit}
|]
    title :: T.Text
    title = docsTitle $ target pkg
    footer :: Html
    footer = [shamlet|
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
    emailText :: E.EmailAddress -> T.Text
    emailText = decodeUtf8 . E.toByteString

moduleTitle :: Module -> Maybe Html
moduleTitle Module { docs = docs' } = do
    d <- docs'
    t <- D.title d
    nodes <- case t of
                 Heading _ inlines _ ->
                    Just $ filterReferences inlines
                 _ -> Nothing
    return $ preEscapedToMarkup $ renderInlines nodes

stylesheet :: TL.Text
stylesheet = renderCss ([cassius|
@import url(#{fontUrl})
body
    padding: 0
    margin: 0
    font-family: Source Sans Pro
    color: #{gray8}
article
    line-height: 1.3
code
    font-family: Source Code Pro
    font-weight: 300
    background-color: #{gray1}
strong code
    font-weight: 400
pre
    padding: 16px 10px
    background-color: #{gray1}
    code
        background: none
div
    border-top: 1px solid #{gray3}
h1, h2, h3, h4, h5, h6
    code
        background-color: #{gray3}
h1, h2, h3, h4, h5, h6, dt
    font-weight: bold
    code
        font-weight: 400
    a.anchor
        text-decoration: none;
        font-family: sans-serif
        color: #{gray3}
        display: none
h1:hover, h2:hover, h3:hover, h4:hover, h5:hover, h6:hover, dt:hover
    a.anchor
        display: inline-block
    a.anchor:hover
        color: #{gray8}
a
    text-decoration: none
a:link
    color: #{indigo8}
a:visited
    color: #{graph8}
a:hover
    text-decoration: underline
dd
    p
        margin-top: 0

nav
    position: fixed
    left: 0
    top: 0
    bottom: 0
    width: #{navWidth}
    overflow-y: auto
    border-right: 1px solid #{gray2}
    background-color: #{gray0}
    code
        background: none
    a.index, ul.toc li a
        display: block
        padding: 0 1em
        margin: 1em 0
        overflow: hidden
        white-space: nowrap
        text-overflow: ellipsis
        color: #{gray8}
    .selected > a, a.selected
        color: #{indigo8} !important
    ul.toc
        margin: 0
        padding: 0
        border-top: 1px solid #{gray2}
        li
            display: block
article, footer
    margin-left: #{navWidth}
    padding: 1em
    > h1, > h2, > h3, > h4, > h5, > h6, > dl
        &:first-child
            margin-top: 0
footer
    border-top: 1px solid #{gray2}
|] undefined)
  where
    fontUrl :: T.Text
    fontUrl = T.concat
        [ "https://fonts.googleapis.com/css"
        , "?family=Source+Code+Pro:300,400|Source+Sans+Pro"
        ]
    -- from Open Color https://yeun.github.io/open-color/
    gray0 :: Color
    gray0 = Color 0xf8 0xf9 0xfa
    gray1 :: Color
    gray1 = Color 0xf1 0xf3 0xf5
    gray2 :: Color
    gray2 = Color 0xe9 0xec 0xef
    gray3 :: Color
    gray3 = Color 0xde 0xe2 0xe6
    gray8 :: Color
    gray8 = Color 0x34 0x3a 0x40
    graph8 :: Color
    graph8 = Color 0x9c 0x36 0xb5
    indigo8 :: Color
    indigo8 = Color 0x3b 0x5b 0xdb
    navWidth :: PixelSize
    navWidth = PixelSize 300

compilePackage' :: Package Docs -> Map FilePath (Either Error BS.ByteString)
compilePackage' pkg = unions
    [ fromList
        [ ("style.css", Right $ encodeUtf8 css)
        , ( "index.html"
          , Right $ toStrict $ renderHtml $ case readme of
                Nothing -> contents pkg
                Just readme' -> readmePage pkg readme'
          )
        ]
    , fromList
        [ ( makeFilePath $ modulePath m
          , Right $ toStrict $ renderHtml $ module' m
          )
        | m <- modules'
        ]
    , mapKeys documentHtmlPath $
        fmap
            (Right . toStrict . renderHtml)
            (mapWithKey (documentPage pkg) documents')
    ]
  where
    paths' :: [ModulePath]
    paths' = MS.keys $ modules pkg
    modules' :: [BoundModule Docs]
    modules' = mapMaybe (`resolveBoundModule` pkg) paths'
    css = T.concat [TL.toStrict stylesheet, "\n\n", docsStyle $ target pkg]
    (documents', readme) = listDocuments pkg

listDocuments :: Package Docs -> (Map FilePath D.Docs, Maybe D.Docs)
listDocuments Package { documents = documents' } =
    case Data.List.break (isReadme . fst) pairs of
        (a, (_, readme) : b) -> (fromList (a ++ b), Just readme)
        (a, []) -> (fromList a, Nothing)
  where
    isReadme :: FilePath -> Bool
    isReadme fp = "readme.md" == map toLower (takeFileName fp)
    pairs :: [(FilePath, D.Docs)]
    pairs = toList documents'

instance Target Docs where
    type CompileResult Docs = BS.ByteString
    type CompileError Docs = Error
    targetName _ = "docs"
    parseTarget table = do
        title <- stringField "title" table
        style <- optional $ stringField "style" table
        header <- optional $ stringField "header" table
        footer <- optional $ stringField "footer" table
        return Docs
            { docsTitle = title
            , docsStyle = fromMaybe "" style
            , docsHeader = fromMaybe "" header
            , docsFooter = fromMaybe "" footer
            }
    compilePackage = compilePackage'
    showCompileError _ = id
    toByteString _ = id
