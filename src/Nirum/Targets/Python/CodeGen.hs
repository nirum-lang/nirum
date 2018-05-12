{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Nirum.Targets.Python.CodeGen
    ( Code
    , CodeGen
    , CodeGenContext (..)
    , CompileError
    , Python (..)
    , PythonVersion (..)
    , RenameMap
    , empty
    , getPythonVersion
    , importBuiltins
    , importStandardLibrary
    , importTypingForPython3
    , indent
    , insertLocalImport
    , insertStandardImport
    , insertStandardImportA
    , insertThirdPartyImports
    , insertThirdPartyImportsA
    , keywords
    , localImportsMap
    , mangleVar
    , minimumRuntime
    , renameModulePath
    , renameModulePath'
    , runCodeGen
    , stringLiteral
    , toAttributeName
    , toAttributeName'
    , toClassName
    , toClassName'
    , toImportPath'
    , toImportPath
    , toImportPaths
    ) where

import Control.Monad.State
import Data.Typeable
import GHC.Exts

import Data.Map.Strict hiding (empty, member, toAscList)
import Data.SemVer hiding (Identifier)
import Data.Set hiding (empty)
import Data.Text hiding (empty)
import qualified Data.Text
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy
import Text.Blaze (ToMarkup (preEscapedToMarkup))
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Printf (printf)

import qualified Nirum.CodeGen
import Nirum.Constructs.Identifier
import Nirum.Constructs.ModulePath
import Nirum.Constructs.Name

minimumRuntime :: Version
minimumRuntime = version 0 6 0 [] []

-- | The set of Python reserved keywords.
-- See also: https://docs.python.org/3/reference/lexical_analysis.html#keywords
keywords :: Set Code
keywords = [ "False", "None", "True"
           , "and", "as", "assert", "break", "class", "continue"
           , "def", "del" , "elif", "else", "except", "finally"
           , "for", "from", "global", "if", "import", "in", "is"
           , "lambda", "nonlocal", "not", "or", "pass", "raise"
           , "return", "try", "while", "with", "yield"
           ]

type RenameMap = Map ModulePath ModulePath

data Python = Python { packageName :: Text
                     , minimumRuntimeVersion :: Version
                     , renames :: RenameMap
                     , classifiers :: [Text]
                     } deriving (Eq, Ord, Show, Typeable)

data PythonVersion = Python2
                   | Python3
                   deriving (Eq, Ord, Show)

type CompileError = Text
type Code = Text

data CodeGenContext
    = CodeGenContext { standardImports :: Map Text Text
                     , standardImportSet :: Set Text
                     , thirdPartyImports :: Map Text (Map Text Text)
                     , localImports :: Map Text (Set Text)
                     , pythonVersion :: PythonVersion
                     }
    deriving (Eq, Ord, Show)

instance Nirum.CodeGen.Failure CodeGenContext CompileError where
    fromString = return . pack

empty :: PythonVersion -> CodeGenContext
empty pythonVer = CodeGenContext
    { standardImports = []
    , standardImportSet = []
    , thirdPartyImports = []
    , localImports = []
    , pythonVersion = pythonVer
    }

localImportsMap :: CodeGenContext -> Map Text (Map Text Text)
localImportsMap CodeGenContext { localImports = imports } =
    Data.Map.Strict.map (Data.Map.Strict.fromSet id) imports

type CodeGen = Nirum.CodeGen.CodeGen CodeGenContext CompileError

runCodeGen :: CodeGen a
           -> CodeGenContext
           -> (Either CompileError a, CodeGenContext)
runCodeGen = Nirum.CodeGen.runCodeGen

importStandardLibrary :: Text -> CodeGen Code
importStandardLibrary module' = do
    insertStandardImportA alias module'
    return alias
  where
    alias :: Code
    alias
      | "_" `isPrefixOf` module' = module'
      | otherwise = '_' `cons` Data.Text.replace "." "_" module'

importBuiltins :: CodeGen Code
importBuiltins = do
    pyVer <- getPythonVersion
    case pyVer of
        Python3 -> do
            insertStandardImportA "__builtin__" "builtins"
            return "__builtin__"
        Python2 -> importStandardLibrary "__builtin__"

insertStandardImport :: Text -> CodeGen ()
insertStandardImport module' =
    insertStandardImportA module' module'

insertStandardImportA :: Code -> Text -> CodeGen ()
insertStandardImportA alias module' = modify insert'
  where
    insert' c@CodeGenContext { standardImports = si, standardImportSet = ss } =
        c { standardImports = Data.Map.Strict.insert alias module' si
          , standardImportSet = Data.Set.insert module' ss
          }

insertThirdPartyImports :: [(Text, Set Text)] -> CodeGen ()
insertThirdPartyImports imports =
    insertThirdPartyImportsA [ (from, Data.Map.Strict.fromSet id objects)
                             | (from, objects) <- imports
                             ]

insertThirdPartyImportsA :: [(Text, Map Text Text)] -> CodeGen ()
insertThirdPartyImportsA imports =
    modify insert'
  where
    insert' c@CodeGenContext { thirdPartyImports = ti } =
        c { thirdPartyImports = Prelude.foldl (unionWith Data.Map.Strict.union)
                                              ti
                                              importList
          }
    importList :: [Map Text (Map Text Text)]
    importList = [ Data.Map.Strict.singleton from objects
                 | (from, objects) <- imports
                 ]

insertLocalImport :: Text -> Text -> CodeGen ()
insertLocalImport module' object = modify insert'
  where
    insert' c@CodeGenContext { localImports = li } =
        c { localImports = insertWith Data.Set.union module' [object] li }

importTypingForPython3 :: CodeGen ()
importTypingForPython3 = do
    pyVer <- getPythonVersion
    case pyVer of
        Python2 -> return ()
        Python3 -> insertStandardImport "typing"

getPythonVersion :: CodeGen PythonVersion
getPythonVersion = fmap pythonVersion Control.Monad.State.get

renameModulePath :: RenameMap -> ModulePath -> ModulePath
renameModulePath renameMap path' =
    rename (Data.Map.Strict.toDescList renameMap)
    -- longest paths should be processed first
  where
    rename :: [(ModulePath, ModulePath)] -> ModulePath
    rename ((from, to) : xs) = let r = replacePrefix from to path'
                               in if r == path'
                                  then rename xs
                                  else r
    rename [] = path'

renameModulePath' :: Python -> ModulePath -> ModulePath
renameModulePath' Python { renames = table } = renameModulePath table

toImportPath' :: ModulePath -> Text
toImportPath' =
    intercalate "." . fmap toAttributeName . GHC.Exts.toList

toImportPath :: Python -> ModulePath -> Text
toImportPath target' = toImportPath' . renameModulePath' target'

toImportPaths :: Python -> Set ModulePath -> [Text]
toImportPaths target' paths =
    toAscList $ Data.Set.map toImportPath' $ hierarchies renamedPaths
  where
    renamedPaths :: Set ModulePath
    renamedPaths = Data.Set.map (renameModulePath' target') paths

toClassName :: Identifier -> Text
toClassName identifier =
    if className `member` keywords then className `snoc` '_' else className
  where
    className :: Text
    className = toPascalCaseText identifier

toClassName' :: Name -> Text
toClassName' = toClassName . facialName

toAttributeName :: Identifier -> Text
toAttributeName identifier =
    if attrName `member` keywords then attrName `snoc` '_' else attrName
  where
    attrName :: Text
    attrName = toSnakeCaseText identifier

toAttributeName' :: Name -> Text
toAttributeName' = toAttributeName . facialName

mangleVar :: Code -> Text -> Code
mangleVar expr arbitrarySideName = Data.Text.concat
    [ "__nirum_"
    , (`Data.Text.map` expr) $ \ c ->
          if 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z' || c == '_'
          then c
          else '_'
    , "__"
    , arbitrarySideName
    , "__"
    ]

-- | Indent the given code.  If there are empty lines these are not indented.
indent :: ToMarkup m => Code -> m -> Code
indent space =
    intercalate "\n"
        . fmap indentLn
        . Data.Text.Lazy.split (== '\n')
        . renderMarkup
        . preEscapedToMarkup
  where
    indentLn :: Data.Text.Lazy.Text -> Code
    indentLn line
      | Data.Text.Lazy.null line = Data.Text.empty
      | otherwise = space `append` toStrict line

stringLiteral :: Text -> Code
stringLiteral string =
    open $ Data.Text.concatMap esc string `snoc` '"'
  where
    open :: Text -> Text
    open =
        if Data.Text.any (> '\xff') string
        then Data.Text.append "u\""
        else Data.Text.cons '"'
    esc :: Char -> Text
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\t' = "\\t"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc c
        | c >= '\x10000' = pack $ printf "\\U%08x" c
        | c >= '\xff' = pack $ printf "\\u%04x" c
        | c < ' ' || c >= '\x7f' = pack $ printf "\\x%02x" c
        | otherwise = Data.Text.singleton c
