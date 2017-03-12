{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings, QuasiQuotes #-}
module Nirum.DocsSpec where

import Data.Text (Text)
import Test.Hspec.Meta
import Text.InterpolatedString.Perl6 (q)

import Nirum.Docs ( Block (..)
                  , HeadingLevel (..)
                  , Inline (..)
                  , ItemList (..)
                  , ListDelimiter (..)
                  , ListType (..)
                  , filterReferences
                  , headingLevelFromInt
                  , parse
                  )

sampleSource :: Text
sampleSource = [q|
Hello
=====

Tight list:

- List test
- test2

Loose list:

1. a

2. b

A [complex *link*][1].

[1]: http://nirum.org/ "Nirum"

|]

sampleDocument :: Block
sampleDocument =
    Document
        [ Heading H1 ["Hello"]
        , Paragraph ["Tight list:"]
        , List BulletList $ TightItemList [ ["List test"]
                                          , ["test2"]
                                          ]
        , Paragraph ["Loose list:"]
        , List (OrderedList 1 Period) $
               LooseItemList [ [Paragraph ["a"]]
                             , [Paragraph ["b"]]
                             ]
        , Paragraph
              [ "A "
              , Link "http://nirum.org/" "Nirum"
                     ["complex ", Emphasis ["link"]]
              , "."
              ]
        ]

spec :: Spec
spec = do
    describe "HeadingLevel" $
        specify "headingLevelFromInt" $ do
            headingLevelFromInt (-1) `shouldBe` H1
            headingLevelFromInt 0 `shouldBe` H1
            headingLevelFromInt 1 `shouldBe` H1
            headingLevelFromInt 2 `shouldBe` H2
            headingLevelFromInt 3 `shouldBe` H3
            headingLevelFromInt 4 `shouldBe` H4
            headingLevelFromInt 5 `shouldBe` H5
            headingLevelFromInt 6 `shouldBe` H6
            headingLevelFromInt 7 `shouldBe` H6
            headingLevelFromInt 99 `shouldBe` H6
    specify "parse" $
        parse sampleSource `shouldBe` sampleDocument
    specify "filterReferences" $
        filterReferences [ "A paragraph that consists of a "
                         , Link "http://nirum.org/" ""
                                [ Emphasis ["hyper"], " link" ]
                         , " and an "
                         , Image "./img.png" "image"
                         , "."
                         ]
            `shouldBe`
                [ "A paragraph that consists of a "
                , Emphasis ["hyper"]
                , " link"
                , " and an "
                , "image"
                , "."
                ]
