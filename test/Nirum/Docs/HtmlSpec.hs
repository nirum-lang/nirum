{-# LANGUAGE QuasiQuotes #-}
module Nirum.Docs.HtmlSpec where

import Test.Hspec.Meta
import Text.InterpolatedString.Perl6 (q)

import Nirum.Docs (Html)
import Nirum.Docs.Html (render)
import Nirum.DocsSpec (sampleDocument)

expectedHtml :: Html
expectedHtml = [q|<h1>Hello</h1>
<p>Tight list:</p>
<ul>
<li>List test</li>
<li>test2</li>
</ul>
<p>Loose list:</p>
<ol>
<li><p>a</p></li>
<li><p>b</p></li>
</ol>
<p>A <a href="http://nirum.org/" title="Nirum">complex <em>link</em></a>.</p>
<table>
<thead>
<tr>

<th>A</th>
<th align="left">B</th>
<th align="center">C</th>
<th align="right">D</th>
</tr>
</thead>
<tbody>
<tr>
<td>foo</td>
<td>bar</td>
<td>baz</td>
<td>bim</td></tr>
<tr>
<td>qux</td>
<td>quux</td>
<td><img src="img.jpg"></td>
<td>quuz</td></tr>
<tr>
<td>corge</td>
<td>grault</td>
<td>garply</td>
<td>waldo</td></tr>
<tr>
<td>ga</td>
<td>na</td>
<td>da</td>
<td>ra</td></tr></tbody></table>
|]

spec :: Spec
spec =
    describe "Docs.Html" $
        specify "render" $
            render sampleDocument `shouldBe` expectedHtml
