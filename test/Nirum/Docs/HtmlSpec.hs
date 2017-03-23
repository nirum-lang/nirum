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
|]

spec :: Spec
spec =
    describe "Docs.Html" $
        specify "render" $
            render sampleDocument `shouldBe` expectedHtml
