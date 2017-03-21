{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Nirum.Docs.ReStructuredText (ReStructuredText, render) where

import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Docs

type ReStructuredText = T.Text

renderInline :: Inline -> ReStructuredText
renderInline (Text t) = escape t
renderInline SoftLineBreak = "\n"
renderInline HardLineBreak = "\n"
renderInline (HtmlInline html) = [qq|:raw:`$html`|]
renderInline (Code code') = [qq|``{code'}``|]
renderInline (Emphasis inlines) = [qq|*{escape $ bareText inlines}*|]
renderInline (Strong inlines) = [qq|**{escape $ bareText inlines}**|]
renderInline (Image url title)
  | T.null title = T.concat ["\n\n.. image:: ", url, "\n\n"]
  | otherwise = T.concat ["\n\n.. image:: ", url, "\n   :alt: ", title, "\n\n"]
renderInline (Link url _ inlines)
  | length images < length inlines = [qq|`{escape $ bareText inlines} <$url>`_|]
  | otherwise = T.replace "\n\n\n\n" "\n\n" $ T.concat [image i | i <- images]
  where
    images :: [(T.Text, T.Text)]
    images = [(url', title) | Image url' title <- inlines]
    image :: (T.Text, T.Text) -> ReStructuredText
    image (url', title)
     | T.null title = T.concat [ "\n\n.. image:: ", url', "\n   :target: "
                               , url, "\n\n"
                               ]
     | otherwise = T.concat ["\n\n.. image:: ", url', "\n   :alt: ", title
                            , "\n   :target: ", url, "\n\n"]

bareText :: [Inline] -> T.Text
bareText inlines =
    T.concat $ map t inlines
  where
    t :: Inline -> T.Text
    t (Text t') = t'
    t SoftLineBreak = "\n"
    t HardLineBreak = "\n"
    t (HtmlInline _) = ""
    t (Code code') = code'
    t (Emphasis inlines') = bareText inlines'
    t (Strong inlines') = bareText inlines'
    t (Link _ _ inlines') = bareText inlines'
    t (Image _ _) = ""

escape :: T.Text -> ReStructuredText
escape = T.concatMap escapeChar

escapeChar :: Char -> Html
escapeChar '\\' = "\\\\"
escapeChar ':' = "\\:"
escapeChar '`' = "\\`"
escapeChar '.' = "\\."
escapeChar c = T.singleton c

renderInlines :: [Inline] -> ReStructuredText
renderInlines inlines =
    T.concat $ prependBar $ map renderInline inlines
  where
    useLineblocks :: Bool
    useLineblocks = not $ null [i | i@HardLineBreak <- inlines]
    prependBar :: [ReStructuredText] -> [ReStructuredText]
    prependBar ts = if useLineblocks then "| " : ts else ts

indent :: T.Text -> ReStructuredText -> ReStructuredText
indent spaces =
    T.intercalate "\n" . map indent' . T.lines
  where
    indent' :: T.Text -> T.Text
    indent' line
      | T.null line = T.empty
      | otherwise = spaces `T.append` line

indent2 :: ReStructuredText -> ReStructuredText
indent2 = indent "  "

indent3 :: ReStructuredText -> ReStructuredText
indent3 = indent "   "

indent4 :: ReStructuredText -> ReStructuredText
indent4 = indent "    "

renderBlock :: Block -> ReStructuredText
renderBlock (Document blocks) = renderBlocks blocks `T.snoc` '\n'
renderBlock ThematicBreak = "----------"
renderBlock (Paragraph inlines) = renderInlines inlines
renderBlock (BlockQuote blocks) = indent4 (renderBlocks blocks)
renderBlock (HtmlBlock html) =
    T.concat [ ".. raw:: html\n\n"
             , indent3 html
             ]
renderBlock (CodeBlock lang code') =
    T.concat [ if T.null lang then "::" else [qq|.. code:: $lang|]
             , "\n\n"
             , indent3 code'
             ]
renderBlock (Heading level inlines) =
    T.concat [text, "\n", T.pack [hChar | _ <- [1 .. (T.length text)]]]
  where
    text :: ReStructuredText
    text = renderInlines inlines
    hChar :: Char
    hChar = case level of
        H1 -> '='
        H2 -> '-'
        H3 -> '~'
        H4 -> '`'
        H5 -> '.'
        H6 -> '\''
renderBlock (List BulletList (TightItemList items)) =
    T.intercalate "\n" [[qq|- {renderInlines i}|] | i <- items]
renderBlock (List BulletList (LooseItemList items)) =
    T.intercalate "\n\n" [ [qq|- {T.drop 2 $ indent2 $ renderBlocks i}|]
                         | i <- items
                         ]
renderBlock (List (OrderedList startNum _) (TightItemList items)) =
    T.intercalate "\n" [ [qq|$n. {renderInlines i}|]
                       | (n, i) <- indexed startNum items
                       ]
renderBlock (List (OrderedList startNum _) (LooseItemList items)) =
    T.intercalate "\n\n" [ [qq|$n. {T.drop 3 $ indent3 $ renderBlocks i}|]
                         | (n, i) <- indexed startNum items
                         ]

indexed :: Enum i => i -> [a] -> [(i, a)]
indexed _ [] = []
indexed start (x : xs) = (start, x) : indexed (succ start) xs

renderBlocks :: [Block] -> ReStructuredText
renderBlocks = T.intercalate "\n\n" . map renderBlock

render :: Block -> ReStructuredText
render = renderBlock
