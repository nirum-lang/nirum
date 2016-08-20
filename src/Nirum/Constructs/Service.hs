module Nirum.Constructs.Service ( Method ( Method
                                         , methodAnnotations
                                         , methodName
                                         , parameters
                                         , returnType
                                         )
                                , Parameter(Parameter)
                                , Service(Service, methods)
                                , methodDocs
                                ) where

import qualified Data.Text as T

import Nirum.Constructs (Construct(toCode))
import Nirum.Constructs.Annotation (AnnotationSet, lookupDocs)
import Nirum.Constructs.Declaration (Declaration(name, docs))
import Nirum.Constructs.Docs (Docs, toCodeWithPrefix)
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
                     , errorType  :: Maybe TypeExpression
                     , methodAnnotations :: AnnotationSet
                     } deriving (Eq, Ord, Show)

methodDocs :: Method -> Maybe Docs
methodDocs = lookupDocs . methodAnnotations

instance Construct Method where
    toCode method@Method { parameters = params
                         , errorType = error'
                         , methodAnnotations = annotationSet'
                         } =
        T.concat $ [ toCode annotationSet'
                   , toCode $ returnType method
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
                   ++ [")"]
                   ++ case error' of
                          Nothing -> []
                          Just e -> [" throws ", toCode e]
                   ++ [","]
      where
        params' :: [Parameter]
        params' = toList params
        docs' :: Maybe Docs
        docs' = lookupDocs annotationSet'
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
