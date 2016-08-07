{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Nirum.Constructs.ServiceSpec where

import Test.Hspec.Meta

import Nirum.Constructs.Declaration (toCode)
import Nirum.Constructs.Service (Method(Method), Parameter(Parameter))
import Nirum.Constructs.TypeExpression ( TypeExpression ( ListModifier
                                                        , OptionModifier
                                                        )
                                       )

spec :: Spec
spec = do
    describe "Parameter" $
        specify "toCode" $ do
            toCode (Parameter "dob" "date" Nothing) `shouldBe` "date dob,"
            toCode (Parameter "dob" "date" $ Just "docs...") `shouldBe`
                "date dob,\n# docs..."
    describe "Method" $
        specify "toCode" $ do
            toCode (Method "ping" [] "bool" Nothing) `shouldBe`
                "bool ping (),"
            toCode (Method "ping" [] "bool" $ Just "docs...") `shouldBe`
                "bool ping (\n  # docs...\n),"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" Nothing]
                           (OptionModifier "user")
                           Nothing) `shouldBe` "user? get-user (uuid user-id),"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" Nothing]
                           (OptionModifier "user")
                           $ Just "docs...") `shouldBe`
                "user? get-user (\n  # docs...\n  uuid user-id,\n),"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" $ Just "param docs..."]
                           (OptionModifier "user")
                           Nothing) `shouldBe`
                "user? get-user (\n  uuid user-id,\n  # param docs...\n),"
            toCode (Method "get-user"
                           [Parameter "user-id" "uuid" $ Just "param docs..."]
                           (OptionModifier "user")
                           $ Just "docs...") `shouldBe`
                "user? get-user (\n\
                \  # docs...\n\
                \  uuid user-id,\n\
                \  # param docs...\n\
                \),"
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" Nothing
                           , Parameter "keyword" "text" Nothing
                           ]
                           (ListModifier "post")
                           Nothing) `shouldBe`
                "[post] search-posts (\n  uuid blog-id,\n  text keyword,\n),"
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" Nothing
                           , Parameter "keyword" "text" Nothing
                           ]
                           (ListModifier "post")
                           $ Just "docs...") `shouldBe`
                "[post] search-posts (\n\
                \  # docs...\n\
                \  uuid blog-id,\n\
                \  text keyword,\n\
                \),"
            toCode (Method "search-posts"
                           [ Parameter "blog-id" "uuid" $ Just "blog id..."
                           , Parameter "keyword" "text" $ Just "keyword..."
                           ]
                           (ListModifier "post")
                           Nothing) `shouldBe`
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
                           $ Just "docs...") `shouldBe`
                "[post] search-posts (\n\
                \  # docs...\n\
                \  uuid blog-id,\n\
                \  # blog id...\n\
                \  text keyword,\n\
                \  # keyword...\n\
                \),"
