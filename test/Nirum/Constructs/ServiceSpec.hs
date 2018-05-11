{-# LANGUAGE OverloadedLists, QuasiQuotes #-}
module Nirum.Constructs.ServiceSpec where

import Data.String.QQ (s)
import Data.Map.Strict as Map (fromList)
import Test.Hspec.Meta

import Nirum.Constructs.Annotation
import Nirum.Constructs.Annotation.Internal
import Nirum.Constructs.Docs (toCode)
import Nirum.Constructs.Service (Method (Method), Parameter (Parameter))
import Nirum.Constructs.TypeExpression ( TypeExpression ( ListModifier
                                                        , OptionModifier
                                                        )
                                       )
import Util (singleDocs)


spec :: Spec
spec = do
    let methodAnno = singleton $ Annotation "http" $ Map.fromList
            [ ("method", AText "GET")
            , ("path", AText "/ping/")
            ]
    let docsAnno = singleDocs "docs..."
    describe "Parameter" $
        specify "toCode" $ do
            toCode (Parameter "dob" "date" empty) `shouldBe` "date dob,"
            toCode (Parameter "dob" "date" docsAnno) `shouldBe`
                "date dob,\n# docs..."
    describe "Method" $
        specify "toCode" $ do
            toCode (Method "ping" [] (Just "bool")
                           Nothing
                           empty) `shouldBe`
                "bool ping (),"
            toCode (Method "ping" [] (Just "bool")
                           Nothing
                           methodAnno) `shouldBe`
                "@http(method = \"GET\", path = \"/ping/\")\nbool ping (),"
            toCode (Method "ping" [] (Just "bool")
                           Nothing
                           docsAnno) `shouldBe`
                "bool ping (\n  # docs...\n),"
            toCode (Method "ping" [] (Just "bool")
                           (Just "ping-error")
                           empty) `shouldBe`
                "bool ping () throws ping-error,"
            toCode (Method "ping" [] (Just "bool")
                           (Just "ping-error")
                           docsAnno) `shouldBe`
                "bool ping (\n  # docs...\n) throws ping-error,"
            toCode (Method "ping" [] (Just "bool")
                           Nothing
                           methodAnno) `shouldBe`
                "@http(method = \"GET\", path = \"/ping/\")\nbool ping (),"
            toCode (Method "ping" [] (Just "bool")
                           (Just "ping-error")
                           methodAnno) `shouldBe`
                "@http(method = \"GET\", path = \"/ping/\")\n\
                \bool ping () throws ping-error,"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" empty]
                           (Just $ OptionModifier "user")
                           Nothing
                           empty) `shouldBe`
                "user? get-user (uuid user-id),"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" empty]
                           (Just $ OptionModifier "user")
                           Nothing
                           docsAnno) `shouldBe`
                "user? get-user (\n  # docs...\n  uuid user-id,\n),"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" empty]
                           (Just $ OptionModifier "user")
                           (Just "get-user-error")
                           empty) `shouldBe`
                "user? get-user (uuid user-id) throws get-user-error,"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" empty]
                           (Just $ OptionModifier "user")
                           (Just "get-user-error")
                           docsAnno) `shouldBe` [s|
user? get-user (
  # docs...
  uuid user-id,
) throws get-user-error,|]
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" $
                                      singleDocs "param docs..."]
                           (Just $ OptionModifier "user")
                           Nothing
                           empty) `shouldBe`
                "user? get-user (\n  uuid user-id,\n  # param docs...\n),"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" $
                                      singleDocs "param docs..."]
                           (Just $ OptionModifier "user")
                           Nothing
                           docsAnno) `shouldBe` [s|
user? get-user (
  # docs...
  uuid user-id,
  # param docs...
),|]
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" $
                                      singleDocs "param docs..."]
                           (Just $ OptionModifier "user")
                           (Just "get-user-error")
                           empty) `shouldBe` [s|
user? get-user (
  uuid user-id,
  # param docs...
) throws get-user-error,|]
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" $
                                      singleDocs "param docs..."]
                           (Just $ OptionModifier "user")
                           (Just "get-user-error")
                           docsAnno) `shouldBe` [s|
user? get-user (
  # docs...
  uuid user-id,
  # param docs...
) throws get-user-error,|]
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" empty
                           , Parameter "keyword" "text" empty
                           ]
                           (Just $ ListModifier "post")
                           Nothing
                           empty) `shouldBe`
                "[post] search-posts (\n  uuid blog-id,\n  text keyword,\n),"
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" empty
                           , Parameter "keyword" "text" empty
                           ]
                           (Just $ ListModifier "post")
                           Nothing
                           docsAnno) `shouldBe` [s|
[post] search-posts (
  # docs...
  uuid blog-id,
  text keyword,
),|]
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" empty
                           , Parameter "keyword" "text" empty
                           ]
                           (Just $ ListModifier "post")
                           (Just "search-posts-error")
                           empty) `shouldBe` [s|
[post] search-posts (
  uuid blog-id,
  text keyword,
) throws search-posts-error,|]
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" empty
                           , Parameter "keyword" "text" empty
                           ]
                           (Just $ ListModifier "post")
                           (Just "search-posts-error")
                           docsAnno) `shouldBe` [s|
[post] search-posts (
  # docs...
  uuid blog-id,
  text keyword,
) throws search-posts-error,|]
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" $
                                       singleDocs "blog id..."
                           , Parameter "keyword" "text" $
                                       singleDocs "keyword..."
                           ]
                           (Just $ ListModifier "post")
                           Nothing
                           empty) `shouldBe` [s|
[post] search-posts (
  uuid blog-id,
  # blog id...
  text keyword,
  # keyword...
),|]
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" $
                                       singleDocs "blog id..."
                           , Parameter "keyword" "text" $
                                       singleDocs "keyword..."
                           ]
                           (Just $ ListModifier "post")
                           Nothing
                           docsAnno) `shouldBe` [s|
[post] search-posts (
  # docs...
  uuid blog-id,
  # blog id...
  text keyword,
  # keyword...
),|]
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" $
                                       singleDocs "blog id..."
                           , Parameter "keyword" "text" $
                                       singleDocs "keyword..."
                           ]
                           (Just $ ListModifier "post")
                           (Just "search-posts-error")
                           empty) `shouldBe` [s|
[post] search-posts (
  uuid blog-id,
  # blog id...
  text keyword,
  # keyword...
) throws search-posts-error,|]
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" $
                                       singleDocs "blog id..."
                           , Parameter "keyword" "text" $
                                       singleDocs "keyword..."
                           ]
                           (Just $ ListModifier "post")
                           (Just "search-posts-error")
                           docsAnno) `shouldBe` [s|
[post] search-posts (
  # docs...
  uuid blog-id,
  # blog id...
  text keyword,
  # keyword...
) throws search-posts-error,|]
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" $
                                       singleDocs "blog id..."
                           , Parameter "keyword" "text" $
                                       singleDocs "keyword..."
                           ]
                           (Just $ ListModifier "post")
                           (Just "search-posts-error")
                           (docsAnno `union` methodAnno)) `shouldBe` [s|
@http(method = "GET", path = "/ping/")
[post] search-posts (
  # docs...
  uuid blog-id,
  # blog id...
  text keyword,
  # keyword...
) throws search-posts-error,|]
