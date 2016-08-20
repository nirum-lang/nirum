{-# LANGUAGE OverloadedLists #-}
module Nirum.CliSpec where

import Control.Monad (forM_)

import qualified Data.Map.Strict as M
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text.IO as TI
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec.Meta

import Nirum.Cli (writeFiles)

expectWriteFiles :: FilePath -> [(FilePath, Text)] -> IO [FilePath]
expectWriteFiles tmpDir files = do
    writeFiles tmpDir $ M.fromList [(f, Right c) | (f, c) <- files]
    forM_ files $ \(f, expectedContent) -> do
        content <- TI.readFile (tmpDir </> f)
        content `shouldBe` expectedContent
    fileList <- listDirectory tmpDir
    return $ sort fileList

spec :: Spec
spec =
    describe "writeFiles" $ do
        it "writes root files" $
            withSystemTempDirectory "writeFiles-rootFiles" $ \tmpDir -> do
                let files = [ ("a.txt", "content A")
                            , ("b.txt", "content B")
                            ]
                fileList <- expectWriteFiles tmpDir files
                fileList `shouldBe` sort [f | (f, _) <- files]
        it "makes directories if necessary" $
            withSystemTempDirectory "writeFiles-directories" $ \tmpDir -> do
                let files = [ ("a.txt", "content A")
                            , ("b.txt", "content B")
                            , ("d/a.txt", "content d/a")
                            , ("d/b/c.txt", "content d/b/c")
                            , ("d2/a.txt", "content d2/a")
                            ]
                fileList <- expectWriteFiles tmpDir files
                fileList `shouldBe` ["a.txt", "b.txt", "d", "d2"]
                dFileList <- listDirectory $ tmpDir </> "d"
                sort dFileList `shouldBe` ["a.txt", "b"]
                dBFileList <- listDirectory $ tmpDir </> "d" </> "b"
                dBFileList `shouldBe` ["c.txt"]
                d2FileList <- listDirectory $ tmpDir </> "d2"
                d2FileList `shouldBe` ["a.txt"]
