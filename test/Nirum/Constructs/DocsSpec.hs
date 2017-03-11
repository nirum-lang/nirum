module Nirum.Constructs.DocsSpec where

import Test.Hspec.Meta

import Nirum.Constructs (Construct (..))
import Nirum.Constructs.Docs (Docs (Docs), title, toBlock, toCodeWithPrefix)
import Nirum.Docs ( Block (Document, Heading, Paragraph)
                  , HeadingLevel (H1, H2)
                  , Inline (Code, Emphasis, Text)
                  )

spec :: Spec
spec =
    describe "Docs" $ do
        specify "toBlock" $
            toBlock (Docs "This is a *docs* `Block`.") `shouldBe`
                Document [ Paragraph [ Text "This is a "
                                     , Emphasis [Text "docs"]
                                     , Text " "
                                     , Code "Block"
                                     , Text "."
                                     ]
                         ]
        context "title" $ do
            it "returns Nothing if it doesn't have any heading blocks" $
                title (Docs "This doesn't have any title.") `shouldBe` Nothing
            it "returns Nothing if its first block is not a heading" $
                title (Docs "Its first block is...\n\n# Not a Heading\n")
                    `shouldBe` Nothing
            it "returns Just Heading if its first block is a heading" $ do
                title (Docs "# H1\n\nis its first block!") `shouldBe`
                    Just (Heading H1 [Text "H1"])
                title (Docs "## Not have to be H1\n\nAny lebel can be a title.")
                    `shouldBe` Just (Heading H2 [Text "Not have to be H1"])
            specify "title block can other Inlines than Text" $
                title (Docs "# `<code>`\n\nThe title can consist of.")
                    `shouldBe` Just (Heading H1 [Code "<code>"])
        context "toCode" $ do
            it "has leading sharps every line" $ do
                toCode (Docs "test") `shouldBe` "# test"
                toCode (Docs "test\ntest2") `shouldBe` "# test\n# test2"
            specify "... except of the last EOL" $ do
                toCode (Docs "test\n") `shouldBe` "# test"
                toCode (Docs "test\ntest2\n") `shouldBe` "# test\n# test2"
        context "toCodeWithPrefix" $ do
            it "simply returns an empty string if input is Nothing" $ do
                toCodeWithPrefix "\n" Nothing `shouldBe` ""
                toCodeWithPrefix "(prefix)" Nothing `shouldBe` ""
            it "prepends the given prefix if input is Just Docs" $ do
                toCodeWithPrefix "\n" (Just "docs") `shouldBe` "\n# docs"
                toCodeWithPrefix "(prefix)" (Just "doc\ndoc")
                    `shouldBe` "(prefix)# doc\n# doc"
        context "fromString" $ do
            it "is equivalent to Docs data constructor" $ do
                ("test\n" :: Docs) `shouldBe` Docs "test\n"
                ("test\ntest2\n" :: Docs) `shouldBe` Docs "test\ntest2\n"
            specify "... except it automatically appends EOL if omitted" $ do
                ("test" :: Docs) `shouldBe` Docs "test\n"
                ("test\ntest2" :: Docs) `shouldBe` Docs "test\ntest2\n"
