{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.TypeScript.TypeExpression (compileTypeExpression) where 

import Data.Text

import qualified Nirum.CodeBuilder as CB hiding (CodeBuilder)
import Nirum.Constructs.Identifier
import Nirum.Constructs.TypeDeclaration
import Nirum.Constructs.TypeExpression
import {-# SOURCE #-} Nirum.Targets.TypeScript ()
import Nirum.Targets.TypeScript.Context
import Nirum.TypeInstance.BoundModule


compileTypeExpression :: Maybe TypeExpression -> CodeBuilder TypeScript Text
compileTypeExpression (Just (TypeIdentifier i)) = do
    lookup' <- CB.lookupType i
    case lookup' of
        Missing -> fail $ "undefined identifier: " ++ toString i
        Imported _ _ (PrimitiveType p _) -> compilePrimitiveType p
        Imported _ _ _ -> return "any"
        Local _ -> return $ toPascalCaseText i
compileTypeExpression _ = return "any"


compilePrimitiveType :: PrimitiveTypeIdentifier
                     -> CodeBuilder TypeScript Text
compilePrimitiveType pti =
    case pti of
        Bigint -> return "number"
        -- FIXME
        Decimal -> compilePrimitiveType Bigint
        Int32 -> compilePrimitiveType Bigint
        Int64 -> compilePrimitiveType Bigint
        Float32 -> compilePrimitiveType Bigint 
        Float64 -> compilePrimitiveType Bigint
        Text -> return "string"
        Binary -> compilePrimitiveType Bigint
        -- FIXME
        Date -> return "Date"
        Datetime -> return "Date"
        -- FIXME
        Bool -> return "boolean"
        -- FIXME
        Uuid -> return "string"
        -- FIXME
        Url -> return "string"
