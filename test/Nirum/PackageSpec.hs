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
            let modules = [ (["examples", "builtins"], builtinsM)
                          , (["examples", "product"], productM)
                          , (["examples", "shapes"], shapesM)
                          ]
            package `shouldBe` Package modules
        specify "scanModules" $ do
            let path = "." </> "examples"
            mods <- scanModules "."
            mods `shouldBe`
                [ (["examples", "builtins"], path </> "builtins.nrm")
                , (["examples", "product"], path </> "product.nrm")
                , (["examples", "shapes"], path </> "shapes.nrm")
                ]
            mods' <- scanModules path
            mods' `shouldBe` [ (["builtins"], path </> "builtins.nrm")
                             , (["product"], path </> "product.nrm")
                             , (["shapes"], path </> "shapes.nrm")
                             ]
