module Nirum.Constructs.Service ( Method ( Method
                                         , methodAnnotations
                                         , methodName
                                         , parameters
                                         , returnType
                                         )
                                , Parameter (Parameter)
                                , Service (Service, methods)
                                , methodDocs
                                ) where

import qualified Data.Text as T

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Annotation (AnnotationSet, empty, lookupDocs)
import Nirum.Constructs.Declaration ( Declaration (annotations, name)
                                    , Documented (docs)
                                    )
import Nirum.Constructs.Docs (Docs, toCodeWithPrefix)
import Nirum.Constructs.DeclarationSet (DeclarationSet, toList)
import Nirum.Constructs.Name (Name)
import Nirum.Constructs.TypeExpression (TypeExpression)

-- | 'Method' parameter.
data Parameter = Parameter Name TypeExpression AnnotationSet
                 deriving (Eq, Ord, Show)

instance Construct Parameter where
    toCode p@(Parameter name' typeExpr _) =
        T.concat [ toCode typeExpr
                 , " "
                 , toCode name'
                 , ","
                 , toCodeWithPrefix "\n" (docs p)
                 ]

instance Documented Parameter

instance Declaration Parameter where
    name (Parameter name' _ _) = name'
    annotations (Parameter _ _ anno') = anno'

-- | 'Service' method.
data Method = Method { methodName :: Name
                     , parameters :: DeclarationSet Parameter
                     , returnType :: TypeExpression
                     , errorType :: Maybe TypeExpression
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
                   ] ++ case (docs method, params') of
                            (Nothing, []) -> []
                            (Just _, []) -> ["\n"]
                            (Nothing, [p@(Parameter _ _ anno')]) ->
                                if anno' == empty
                                then [T.dropWhileEnd (== ',') $ toCode p]
                                else verboseForm params'
                            _ -> verboseForm params'
                   ++ [")"]
                   ++ case error' of
                          Nothing -> []
                          Just e -> [" throws ", toCode e]
                   ++ [","]
      where
        params' :: [Parameter]
        params' = toList params
        docs' :: Maybe Docs
        docs' = docs method
        indentedCode :: Construct a => a -> T.Text
        indentedCode c = T.concat [ "  "
                                  , T.intercalate "\n  " $ T.lines (toCode c)
                                  ]
        verboseForm :: [Parameter] -> [T.Text]
        verboseForm p = [ "\n"
                        , T.intercalate "\n" $ map indentedCode p
                        , "\n"
                        ]

instance Documented Method

instance Declaration Method where
    name = methodName
    annotations = methodAnnotations

-- | RPC service.
newtype Service =
    Service { methods :: DeclarationSet Method } deriving (Eq, Ord, Show)
