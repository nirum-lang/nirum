{-# LANGUAGE OverloadedLists #-}
module Nirum.Constructs.DeclarationSetSpec (SampleDecl (..), spec) where

import Control.Exception.Base (evaluate)
import Data.String (IsString (..))

import Test.Hspec.Meta

import Nirum.Constructs (Construct (..))
import qualified Nirum.Constructs.Annotation as A
import Nirum.Constructs.Annotation (AnnotationSet)
import Nirum.Constructs.Declaration (Declaration (..), Documented)
import Nirum.Constructs.DeclarationSet ( DeclarationSet
                                       , NameDuplication (..)
                                       , delete
                                       , empty
                                       , fromList
                                       , lookup'
                                       , null'
                                       , size
                                       , toList
                                       , union
                                       , (!)
                                       )
import Nirum.Constructs.Name (Name (Name))

data SampleDecl = SampleDecl Name AnnotationSet deriving (Eq, Ord, Show)

instance Construct SampleDecl where
    toCode _ = "(do not impl)"

instance Documented SampleDecl

instance Declaration SampleDecl where
    name (SampleDecl name' _) = name'
    annotations (SampleDecl _ anno') = anno'

instance IsString SampleDecl where
    fromString s = SampleDecl (fromString s) A.empty

type SampleDeclSet = DeclarationSet SampleDecl

spec :: Spec
spec =
    describe "DeclarationSet" $ do
        let sd fname bname = SampleDecl (Name fname bname) A.empty
        context "fromList" $ do
            let fl = fromList :: [SampleDecl]
                              -> Either NameDuplication SampleDeclSet
            it ("returns Left FacialNameDuplication "
                ++ "if duplicated facial name exists") $ do
                fl ["foo", "bar", "foo"] `shouldBe`
                    Left (FacialNameDuplication "foo")
                fl ["foo", "bar", "bar"] `shouldBe`
                    Left (FacialNameDuplication "bar")
                fl ["foo", "bar", sd "foo" "baz"] `shouldBe`
                    Left (FacialNameDuplication $ Name "foo" "baz")
            it ("returns Left BehindNameDuplication "
                ++ "if duplicated behind name exists") $ do
                fl ["foo", "bar", sd "baz" "foo"] `shouldBe`
                    Left (BehindNameDuplication $ Name "baz" "foo")
                fl [sd "foo" "bar", "bar", "baz"] `shouldBe`
                    Left (BehindNameDuplication "bar")
            it "returns Right DeclarationSet if there are no duplications" $ do
                let Right dset = fl ["foo", "bar", "baz"]
                toList dset `shouldBe` ["foo", "bar", "baz"]
        let dset = ["foo", "bar", sd "baz" "asdf"] :: SampleDeclSet
        context "toList" $
            it "returns [Declaration]" $ do
                toList dset `shouldBe` ["foo", "bar", sd "baz" "asdf"]
                toList (empty :: SampleDeclSet) `shouldBe` []
        context "size" $
            it "returns its cardinality" $ do
                size dset `shouldBe` 3
                size (empty :: SampleDeclSet) `shouldBe` 0
        context "null" $ do
            it "returns True if the set is empty" $ do
                ([] :: SampleDeclSet) `shouldSatisfy` null'
                (empty :: SampleDeclSet) `shouldSatisfy` null'
            it "returns False if the set is not empty" $
                dset `shouldNotSatisfy` null'
        context "lookup" $ do
            it "returns Nothing if there is no such facial name" $
                lookup' "not-exists" dset `shouldBe` Nothing
            it "returns Just Declaration if the name exists" $ do
                lookup' "foo" dset `shouldBe` Just "foo"
                lookup' "bar" dset `shouldBe` Just "bar"
                lookup' "baz" dset `shouldBe` Just (sd "baz" "asdf")
        context "DeclarationSet ! Identifier" $ do
            it "returns Declaration if the name exists" $ do
                dset ! "foo" `shouldBe` "foo"
                dset ! "bar" `shouldBe` "bar"
                dset ! "baz" `shouldBe` sd "baz" "asdf"
            it "fails if there is no such facial name" $
                evaluate (dset ! "not-exists") `shouldThrow` anyException
        context "union" $ do
            it "returns Right DeclarationSet if there aren't no duplications" $
                union dset ["apple", "banana"] `shouldBe`
                    Right ["foo" , "bar" , sd "baz" "asdf", "apple", "banana"]
            it "returns Left FacialNameDuplication if facial names are dup" $
                union dset [sd "foo" "xyz"] `shouldBe`
                    Left (FacialNameDuplication $ Name "foo" "xyz")
            it "returns Left BehindNameDuplication if behind names are dup" $
                union dset [sd "xyz" "foo"] `shouldBe`
                    Left (BehindNameDuplication $ Name "xyz" "foo")
        specify "delete" $
            delete "bar" dset `shouldBe` ["foo", sd "baz" "asdf"]
