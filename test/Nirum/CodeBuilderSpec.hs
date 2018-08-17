{-# LANGUAGE OverloadedLists, TypeFamilies #-}
module Nirum.CodeBuilderSpec where

import Control.Monad (forM_)
import Data.Map.Strict as M
import qualified Data.SemVer as SV
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import Test.Hspec.Meta
import qualified Text.PrettyPrint as P

import Nirum.CodeBuilder
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Module (Module (..))
import Nirum.Constructs.ModulePath (ModulePath (..))
import qualified Nirum.Constructs.TypeDeclaration as TD
import Nirum.Package.Metadata (Metadata (..), Package (..), Target (..))
import qualified Nirum.Package.ModuleSet as MS
import Nirum.TypeInstance.BoundModule hiding (lookupType)


emptyModule :: Module
emptyModule = Module { types = DS.empty, docs = Nothing }

modules' :: MS.ModuleSet
modules' = case m of
    Right m' -> m'
    _ -> error "unreachable"
  where
    m = MS.fromList [ (["fruits"], emptyModule)
                    , (["imported-commons"], emptyModule)
                    , (["transports", "truck"], emptyModule)
                    , (["transports", "container"], emptyModule)
                    ]

package :: Package DummyTarget
package = Package { metadata = Metadata { version = SV.version 0 0 1 [] []
                                        , authors = []
                                        , description = Nothing
                                        , license = Nothing
                                        , keywords = []
                                        , target = DummyTarget
                                        }
                  , modules = modules'
                  , documents = M.empty
                  }

run :: CodeBuilder DummyTarget () a -> L.Text
run = B.toLazyText . snd . runBuilder package ["fruits"] ()


spec = do
    specify "writeLine" $ do
        let w = do
                writeLine "a"
                writeLine "b"
        run w `shouldBe` "a\nb\n"
    specify "writeLine 2" $ do
        let w = forM_ ([1 .. 10] :: [Integer]) $ \ i ->
                    writeLine $ P.text $ show i
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
    describe "lookupType" $
        specify "primitives" $ do
            let run' = fst . runBuilder package ["fruits"] ()
            let core = ModuleName "core"
            run' (lookupType "text") `shouldBe`
                Imported core "text" (TD.PrimitiveType TD.Text TD.String)
            run' (lookupType "int32") `shouldBe`
                Imported core "int32" (TD.PrimitiveType TD.Int32 TD.Number)


data DummyTarget = DummyTarget deriving (Eq, Ord, Show)

instance Target DummyTarget where
    type CompileResult DummyTarget = T.Text
    type CompileError DummyTarget = T.Text
    targetName _ = "dummy"
    parseTarget _ = return DummyTarget
    compilePackage _ = M.empty
    showCompileError _ e = e
    toByteString _ = encodeUtf8
