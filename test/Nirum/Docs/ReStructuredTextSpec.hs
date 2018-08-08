{-# LANGUAGE QuasiQuotes #-}
module Nirum.Docs.ReStructuredTextSpec where

import Test.Hspec.Meta
import Text.InterpolatedString.Perl6 (q)

import Nirum.Docs.ReStructuredText (ReStructuredText, render)
import Nirum.DocsSpec (sampleDocument)

expectedRst :: ReStructuredText
expectedRst = [q|Hello
=====

Tight list\:

- List test
- test2

Loose list\:

1. a

2. b

A `complex link <http://nirum.org/>`_\.


.. _table-section:

Table example
-------------


+-------+--------+--------------------+-------+
| A     | B      | C                  | D     |
+=======+========+====================+=======+
| foo   | bar    | baz                | bim   |
+-------+--------+--------------------+-------+
| qux   | quux   |                    | quuz  |
|       |        |                    |       |
|       |        | .. image:: img.jpg |       |
|       |        |                    |       |
+-------+--------+--------------------+-------+
| corge | grault | garply             | waldo |
+-------+--------+--------------------+-------+
| ga    | na     | da                 | ra    |
+-------+--------+--------------------+-------+


|]

spec :: Spec
spec =
    describe "Docs.ReStructuredText" $
        specify "render" $
            render sampleDocument `shouldBe` expectedRst
