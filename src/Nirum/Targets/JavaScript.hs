{-# LANGUAGE ExtendedDefaultRules, OverloadedLists, QuasiQuotes,
  TypeSynonymInstances, MultiParamTypeClasses #-}
module Nirum.Targets.JavaScript ( Code
                                , CodeGen
                                , CodeGenContext
                                , CompileError
                                , Source (sourcePackage)
                                , compilePackage
                                , compileModule
                                ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified Nirum.CodeGen as C
import Nirum.Package (Package)

type Code = T.Text
type CompileError = T.Text

data Source = Source { sourcePackage :: Package }
data CodeGenContext = CodeGenContext
                    deriving (Eq, Ord, Show)

type CodeGen = C.CodeGen CodeGenContext CompileError

runCodeGen :: CodeGen a
           -> CodeGenContext
           -> (Either CompileError a, CodeGenContext)
runCodeGen = C.runCodeGen

compilePackage :: Package
               -> M.Map FilePath (Either CompileError Code)
compilePackage package = M.fromList [ ("package.json", Right "world")
                              , ("index.js", compileModule $ Source package )
                              ]

compileModuleBody :: Source -> CodeGen Code
compileModuleBody src = do
    typeCodes <- mapM (compileTypeDeclaration src)

compileModule :: Source -> Either CompileError Code
compileModule source =
    case runCodeGen code' of
        (Left errMsg, _) -> Left errMsg
        (Right code, context) -> code
  where
    code' :: CodeGen Code
    code' = compileModuleBody source