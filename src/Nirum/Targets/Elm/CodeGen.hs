{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Nirum.Targets.Elm.CodeGen
    ( CodeGen
    , CodeGenContext (..)
    , Elm (..)
    , ElmLibrary (..)
    , Error
    , def
    , enumerate
    , import'
    , require
    , runCodeGen
    , runCodeGen'
    , sourceDirectory
    , toFilePath
    , toImportPath
    ) where

import Control.Monad.State
import Data.Maybe
import GHC.Exts (IsList (..))
import Prelude hiding (lookup)

import Data.Map.Strict hiding (toList)
import Data.Set hiding (toList)
import Data.Text hiding (zip)
import System.FilePath
import Text.URI

import qualified Nirum.CodeGen
import Nirum.Constructs.Identifier
import Nirum.Constructs.ModulePath

newtype Elm = Elm { repositoryUrl :: URI } deriving (Eq, Ord, Show)

type Error = Text

data ElmLibrary
    = ElmBytes
    | ElmDecimal
    | ElmNirum
    | ElmUuid
    deriving (Eq, Ord, Show)

data CodeGenContext = CodeGenContext
    { imports :: Map Text Text
    , dependencies :: Set ElmLibrary
    } deriving (Eq, Ord, Show)

instance Nirum.CodeGen.Failure CodeGenContext Error where
    fromString = return . pack

def :: CodeGenContext
def = CodeGenContext [] []

type CodeGen = Nirum.CodeGen.CodeGen CodeGenContext Error

runCodeGen :: CodeGen a
           -> CodeGenContext
           -> (Either Error a, CodeGenContext)
runCodeGen = Nirum.CodeGen.runCodeGen

runCodeGen' :: CodeGen a -> (Either Error a, CodeGenContext)
runCodeGen' = (`runCodeGen` def)

import' :: Text -> CodeGen Text
import' moduleName' = do
    mangled <- gets (lookup moduleName' . imports)
    let mangleName = fromMaybe (replace "." "_" moduleName' `snoc` '_') mangled
    modify $ \ c@CodeGenContext { imports = imports' } ->
        c { imports = Data.Map.Strict.insert moduleName' mangleName imports' }
    return mangleName

require :: ElmLibrary -> CodeGen ()
require lib = modify $ \ c@CodeGenContext { dependencies = deps } ->
    c { dependencies = Data.Set.insert lib deps }

sourceDirectory :: FilePath
sourceDirectory = "src"

toImportPath :: ModulePath -> Text
toImportPath = intercalate "." . fmap toPascalCaseText . toList

toFilePath :: ModulePath -> FilePath
toFilePath =
    (sourceDirectory </>) . (<.> "elm") .
    joinPath . fmap (unpack . toPascalCaseText) . toList

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]
