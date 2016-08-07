{-# LANGUAGE OverloadedStrings #-}
module Nirum.Constructs.Service ( Method ( Method
                                         , methodDocs
                                         , methodName
                                         , parameters
                                         , returnType
                                         )
                                , Parameter(Parameter)
                                , Service(Service, methods)
                                ) where

import qualified Data.Text as T

import Nirum.Constructs (Construct(toCode))
import Nirum.Constructs.Declaration ( Declaration(name, docs)
                                    , Docs
                                    , toCodeWithPrefix
                                    )
import Nirum.Constructs.DeclarationSet (DeclarationSet, toList)
import Nirum.Constructs.Name (Name)
import Nirum.Constructs.TypeExpression (TypeExpression)

-- | 'Method' parameter.
data Parameter = Parameter Name TypeExpression (Maybe Docs)
                 deriving (Eq, Ord, Show)

instance Construct Parameter where
    toCode (Parameter name' typeExpr docs') =
        T.concat [ toCode typeExpr
                 , " "
                 , toCode name'
                 , ","
                 , toCodeWithPrefix "\n" docs'
                 ]

instance Declaration Parameter where
    name (Parameter name' _ _) = name'
    docs (Parameter _ _ docs') = docs'

-- | 'Service' method.
data Method = Method { methodName :: Name
                     , parameters :: DeclarationSet Parameter
                     , returnType :: TypeExpression 
                     , methodDocs :: Maybe Docs
                     } deriving (Eq, Ord, Show)

instance Construct Method where
    toCode method@Method { parameters = params, methodDocs = docs' } =
        T.concat $ [ toCode $ returnType method
                   , " "
                   , toCode $ methodName method
                   , " ("
                   , toCodeWithPrefix "\n  " docs'
                   ] ++ case (docs', params') of
                            (Nothing, []) -> []
                            (Just _, []) -> ["\n"]
                            (Nothing, [p@(Parameter _ _ Nothing)]) ->
                                [T.dropWhileEnd (== ',') $ toCode p]
                            _ -> [ "\n"
                                 , T.intercalate "\n" $ map indentedCode params'
                                 , "\n"
                                 ]
                   ++ ["),"]
      where
        params' :: [Parameter]
        params' = toList params
        indentedCode :: Construct a => a -> T.Text
        indentedCode c = T.concat [ "  "
                                  , T.intercalate "\n  " $ T.lines (toCode c)
                                  ]

instance Declaration Method where
    name = methodName
    docs = methodDocs

-- | RPC service.
data Service =
    Service { methods :: DeclarationSet Method } deriving (Eq, Ord, Show)
