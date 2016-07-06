{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Nirum.PackageSpec where

import System.FilePath ((</>))
import Test.Hspec.Meta

import Nirum.Package (Package(Package), scanModules, scanPackage)
import Nirum.Parser (parseFile)

spec :: Spec
spec =
    describe "Package" $ do
        specify "scanPackage" $ do
            Right package <- scanPackage "."
            let path = "." </> "examples"
            Right builtinsM <- parseFile (path </> "builtins.nrm")
            Right productM <- parseFile (path </> "product.nrm")
            Right shapesM <- parseFile (path </> "shapes.nrm")
            Right countriesM <- parseFile (path </> "countries.nrm")
            Right addressM <- parseFile (path </> "address.nrm")
            let modules = [ (["examples", "builtins"], builtinsM)
                          , (["examples", "product"], productM)
                          , (["examples", "shapes"], shapesM)
                          , (["examples", "countries"], countriesM)
                          , (["examples", "address"], addressM)
                          ]
            package `shouldBe` Package modules
        specify "scanModules" $ do
            let path = "." </> "examples"
            mods <- scanModules "."
            mods `shouldBe`
                [ (["examples", "builtins"], path </> "builtins.nrm")
                , (["examples", "product"], path </> "product.nrm")
                , (["examples", "shapes"], path </> "shapes.nrm")
                , (["examples", "countries"], path </> "countries.nrm")
                , (["examples", "address"], path </> "address.nrm")
                ]
            mods' <- scanModules path
            mods' `shouldBe` [ (["builtins"], path </> "builtins.nrm")
                             , (["product"], path </> "product.nrm")
                             , (["shapes"], path </> "shapes.nrm")
                             , (["countries"], path </> "countries.nrm")
                             , (["address"], path </> "address.nrm")
                             ]
