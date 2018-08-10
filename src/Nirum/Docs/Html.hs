{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Nirum.Docs.Html
    ( render
    , renderBlock
    , renderInline
    , renderInlines
    , renderLinklessInlines
    ) where

import Data.List.NonEmpty
import Prelude hiding (head, zip)

import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Docs

renderInline :: Inline -> Html
renderInline (Text t) = escape t
renderInline SoftLineBreak = "\n"
renderInline HardLineBreak = "<br>"
renderInline (HtmlInline html) = html
renderInline (Code code') = [qq|<code>{escape code'}</code>|]
renderInline (Emphasis inlines) = [qq|<em>{renderInlines inlines}</em>|]
renderInline (Strong inlines) = [qq|<strong>{renderInlines inlines}</strong>|]
renderInline (Link url title inlines) =
    let body = renderInlines inlines
    in
        if T.null title
            then [qq|<a href="{escape url}">$body</a>|]
            else [qq|<a href="{escape url}" title="{escape title}">$body</a>|]
renderInline (Image url title) =
    if T.null title
        then [qq|<img src="{escape url}">|]
        else [qq|<img src="{escape url}" title="{escape title}">|]

escape :: T.Text -> Html
escape = T.concatMap escapeChar

escapeChar :: Char -> Html
escapeChar '&' = "&amp;"
escapeChar '"' = "&quot;"
escapeChar '<' = "&lt;"
escapeChar '>' = "&gt;"
escapeChar c = T.singleton c

renderInlines :: [Inline] -> Html
renderInlines = T.concat . fmap renderInline

renderLinklessInlines :: [Inline] -> Html
renderLinklessInlines inlines = T.concat
    [ case i of
        Link _ _ inlines' -> renderInlines inlines'
        i' -> renderInline i'
    | i <- inlines
    ]

renderBlock :: Block -> Html
renderBlock (Document blocks) = renderBlocks blocks `T.snoc` '\n'
renderBlock ThematicBreak = "<hr>"
renderBlock (Paragraph inlines) = [qq|<p>{renderInlines inlines}</p>|]
renderBlock (BlockQuote blocks) =
    [qq|<blockquote>{renderBlocks blocks}</blockquotes>|]
renderBlock (HtmlBlock html) = html
renderBlock (CodeBlock lang code') =
    if T.null lang
    then [qq|<pre><code>$code'</code></pre>|]
    else [qq|<pre><code class="language-$lang">$code'</code></pre>|]
renderBlock (Heading level inlines anchorId) =
    let lv = headingLevelInt level
        id' = case anchorId of
                Nothing -> ""
                Just aid -> [qq| id="$aid"|] :: T.Text
    in [qq|<h$lv$id'>{renderInlines inlines}</h$lv>|]
renderBlock (List listType itemList) =
    let liList = case itemList of
                     TightItemList items ->
                         [ [qq|<li>{renderTightBlocks item}</li>|]
                         | item <- items
                         ]
                     LooseItemList items ->
                         [ [qq|<li>{renderBlocks item}</li>|]
                         | item <- items
                         ]
        tag = case listType of
                  BulletList -> "ul" :: T.Text
                  OrderedList { startNumber = 1 } -> "ol"
                  OrderedList { startNumber = startNumber' } ->
                      [qq|ol start="$startNumber'"|]
        nl = '\n'
        liListT = T.intercalate "\n" liList
    in [qq|<$tag>$nl$liListT$nl</$tag>|]
renderBlock (Table columns rows) =
    [qq|<table>$lf<thead>$lf<tr>
{T.concat (toList $ fmap th $ zip columns (head rows))}
</tr>$lf</thead>
<tbody>{T.concat (fmap tr $ Data.List.NonEmpty.tail rows)}</tbody></table>|]
  where
    lf :: Char
    lf = '\n'
    th :: (TableColumn, TableCell) -> Html
    th (col, cell) = [qq|$lf<th{align col}>{renderInlines cell}</th>|]
    align :: TableColumn -> Html
    align NotAligned = ""
    align LeftAligned = " align=\"left\""
    align CenterAligned = " align=\"center\""
    align RightAligned = " align=\"right\""
    tr :: TableRow -> Html
    tr cells = [qq|$lf<tr>{T.concat (toList $ fmap td cells)}</tr>|]
    td :: TableCell -> Html
    td inlines = [qq|$lf<td>{renderInlines inlines}</td>|]

renderBlocks :: [Block] -> Html
renderBlocks = T.intercalate "\n" . fmap renderBlock

renderTightBlocks :: [Block] -> Html
renderTightBlocks blocks = T.intercalate "\n"
    [ case b of
        Paragraph inlines -> renderInlines inlines
        b' -> renderBlock b'
    | b <- blocks
    ]

render :: Block -> Html
render = renderBlock
