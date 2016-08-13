{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Nirum.Constructs.ServiceSpec where

import Test.Hspec.Meta

import Nirum.Constructs.Annotation (Annotation (Annotation), empty, fromList)
import Nirum.Constructs.Declaration (toCode)
import Nirum.Constructs.Service (Method(Method), Parameter(Parameter))
import Nirum.Constructs.TypeExpression ( TypeExpression ( ListModifier
                                                        , OptionModifier
                                                        )
                                       )


spec :: Spec
spec = do
    let Right methodAnno = fromList [Annotation "http-get" (Just "/ping/")]
    describe "Parameter" $
        specify "toCode" $ do
            toCode (Parameter "dob" "date" Nothing) `shouldBe` "date dob,"
            toCode (Parameter "dob" "date" $ Just "docs...") `shouldBe`
                "date dob,\n# docs..."
    describe "Method" $
        specify "toCode" $ do
            toCode (Method "ping" [] "bool"
                           Nothing
                           Nothing
                           empty) `shouldBe`
                "bool ping (),"
            toCode (Method "ping" [] "bool"
                           Nothing
                           Nothing
                           methodAnno) `shouldBe`
                "@http-get(\"/ping/\")\nbool ping (),"
            toCode (Method "ping" [] "bool"
                           Nothing
                           (Just "docs...")
                           empty) `shouldBe`
                "bool ping (\n  # docs...\n),"
            toCode (Method "ping" [] "bool"
                           (Just "ping-error")
                           Nothing
                           empty) `shouldBe`
                "bool ping () throws ping-error,"
            toCode (Method "ping" [] "bool"
                           (Just "ping-error")
                           (Just "docs...")
                           empty) `shouldBe`
                "bool ping (\n  # docs...\n) throws ping-error,"
            toCode (Method "ping" [] "bool"
                           Nothing
                           Nothing
                           methodAnno) `shouldBe`
                "@http-get(\"/ping/\")\nbool ping (),"
            toCode (Method "ping" [] "bool"
                           (Just "ping-error")
                           Nothing
                           methodAnno) `shouldBe`
                "@http-get(\"/ping/\")\nbool ping () throws ping-error,"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" Nothing]
                           (OptionModifier "user")
                           Nothing
                           Nothing
                           empty) `shouldBe`
                "user? get-user (uuid user-id),"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" Nothing]
                           (OptionModifier "user")
                           Nothing
                           (Just "docs...")
                           empty) `shouldBe`
                "user? get-user (\n  # docs...\n  uuid user-id,\n),"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" Nothing]
                           (OptionModifier "user")
                           (Just "get-user-error")
                           Nothing
                           empty) `shouldBe`
                "user? get-user (uuid user-id) throws get-user-error,"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" Nothing]
                           (OptionModifier "user")
                           (Just "get-user-error")
                           (Just "docs...")
                           empty) `shouldBe`
                "user? get-user (\n\
                \  # docs...\n\
                \  uuid user-id,\n\
                \) throws get-user-error,"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" $ Just "param docs..."]
                           (OptionModifier "user")
                           Nothing
                           Nothing
                           empty) `shouldBe`
                "user? get-user (\n  uuid user-id,\n  # param docs...\n),"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" $ Just "param docs..."]
                           (OptionModifier "user")
                           Nothing
                           (Just "docs...")
                           empty) `shouldBe`
                "user? get-user (\n\
                \  # docs...\n\
                \  uuid user-id,\n\
                \  # param docs...\n\
                \),"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" $ Just "param docs..."]
                           (OptionModifier "user")
                           (Just "get-user-error")
                           Nothing
                           empty) `shouldBe`
                "user? get-user (\n\
                \  uuid user-id,\n\
                \  # param docs...\n\
                \) throws get-user-error,"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" $ Just "param docs..."]
                           (OptionModifier "user")
                           (Just "get-user-error")
                           (Just "docs...")
                           empty) `shouldBe`
                "user? get-user (\n\
                \  # docs...\n\
                \  uuid user-id,\n\
                \  # param docs...\n\
                \) throws get-user-error,"
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" Nothing
                           , Parameter "keyword" "text" Nothing
                           ]
                           (ListModifier "post")
                           Nothing
                           Nothing
                           empty) `shouldBe`
                "[post] search-posts (\n  uuid blog-id,\n  text keyword,\n),"
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" Nothing
                           , Parameter "keyword" "text" Nothing
                           ]
                           (ListModifier "post")
                           Nothing
                           (Just "docs...")
                           empty) `shouldBe`
                "[post] search-posts (\n\
                \  # docs...\n\
                \  uuid blog-id,\n\
                \  text keyword,\n\
                \),"
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" Nothing
                           , Parameter "keyword" "text" Nothing
                           ]
                           (ListModifier "post")
                           (Just "search-posts-error")
                           Nothing
                           empty) `shouldBe`
                "[post] search-posts (\n\
                \  uuid blog-id,\n\
                \  text keyword,\n\
                \) throws search-posts-error,"
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" Nothing
                           , Parameter "keyword" "text" Nothing
                           ]
                           (ListModifier "post")
                           (Just "search-posts-error")
                           (Just "docs...")
                           empty) `shouldBe`
                "[post] search-posts (\n\
                \  # docs...\n\
                \  uuid blog-id,\n\
                \  text keyword,\n\
                \) throws search-posts-error,"
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" $ Just "blog id..."
                           , Parameter "keyword" "text" $ Just "keyword..."
                           ]
                           (ListModifier "post")
                           Nothing
                           Nothing
                           empty) `shouldBe`
                "[post] search-posts (\n\
                \  uuid blog-id,\n\
                \  # blog id...\n\
                \  text keyword,\n\
                \  # keyword...\n\
                \),"
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" $ Just "blog id..."
                           , Parameter "keyword" "text" $ Just "keyword..."
                           ]
                           (ListModifier "post")
                           Nothing
                           (Just "docs...")
                           empty) `shouldBe`
                "[post] search-posts (\n\
                \  # docs...\n\
                \  uuid blog-id,\n\
                \  # blog id...\n\
                \  text keyword,\n\
                \  # keyword...\n\
                \),"
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" $ Just "blog id..."
                           , Parameter "keyword" "text" $ Just "keyword..."
                           ]
                           (ListModifier "post")
                           (Just "search-posts-error")
                           Nothing
                           empty) `shouldBe`
                "[post] search-posts (\n\
                \  uuid blog-id,\n\
                \  # blog id...\n\
                \  text keyword,\n\
                \  # keyword...\n\
                \) throws search-posts-error,"
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" $ Just "blog id..."
                           , Parameter "keyword" "text" $ Just "keyword..."
                           ]
                           (ListModifier "post")
                           (Just "search-posts-error")
                           (Just "docs...")
                           empty) `shouldBe`
                "[post] search-posts (\n\
                \  # docs...\n\
                \  uuid blog-id,\n\
                \  # blog id...\n\
                \  text keyword,\n\
                \  # keyword...\n\
                \) throws search-posts-error,"
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" $ Just "blog id..."
                           , Parameter "keyword" "text" $ Just "keyword..."
                           ]
                           (ListModifier "post")
                           (Just "search-posts-error")
                           (Just "docs...")
                           methodAnno) `shouldBe`
                "@http-get(\"/ping/\")\n\
                \[post] search-posts (\n\
                \  # docs...\n\
                \  uuid blog-id,\n\
                \  # blog id...\n\
                \  text keyword,\n\
                \  # keyword...\n\
                \) throws search-posts-error,"
