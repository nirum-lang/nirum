module Nirum.CodeBuilderSpec ( spec
                             ) where

import Control.Monad (forM_)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import Test.Hspec.Meta
import qualified Text.PrettyPrint as P

import Nirum.CodeBuilder


run :: CodeBuilder a -> L.Text
run = B.toLazyText . snd . runBuilder


spec = do
    specify "writeLine" $ do
        let w = do
                writeLine "a"
                writeLine "b"
        run w `shouldBe` "a\nb\n"
    specify "writeLine 2" $ do
        let w = forM_ [1 :: Integer .. 10] $ \i -> writeLine $ P.text $ show i
        run w `shouldBe` "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n"
    describe "nest" $ do
        it "should indent its own context" $ do
            let w = do
                    writeLine "a"
                    nest 4 $ writeLine "b"
            run w `shouldBe` "a\n    b\n"
        it "should indent its own context (2)" $ do
            let w = do
                    writeLine "a"
                    nest 4 $ do
                        writeLine "b"
                        writeLine "cd"
                    writeLine "eee"
            run w `shouldBe` "a\n    b\n    cd\neee\n"
