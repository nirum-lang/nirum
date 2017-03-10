{-# LANGUAGE TypeFamilies #-}
module Nirum.Targets.JavaScript ( JavaScript ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Nirum.Package.Metadata (Target ( CompileError
                                       , CompileResult
                                       , compilePackage
                                       , parseTarget
                                       , showCompileError
                                       , targetName
                                       , toByteString
                                       )
                              , stringField
                              )


data JavaScript = JavaScript { packageName :: T.Text }
    deriving (Eq, Ord, Show)

instance Target JavaScript where
    type CompileResult JavaScript = T.Text
    type CompileError JavaScript = ()
    targetName _ = "javascript"
    parseTarget table = do
        name' <- stringField "name" table
        return JavaScript { packageName = name' }
    compilePackage _ = M.empty
    showCompileError _ _e = ""
    toByteString _ _ = BS.empty
