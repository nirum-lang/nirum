{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Nirum.Targets.List ( targetProxiesQ
                          , targetProxyMapQ
                          , targetTypesQ
                          ) where

import Data.Proxy (Proxy (Proxy))
import Language.Haskell.TH ( Dec (InstanceD)
                           , Exp
                           , Info (ClassI)
                           , Q
                           , Type (AppT)
                           )
import Language.Haskell.TH.Lib (listE)
import Language.Haskell.TH.Syntax (reify, sequenceQ)

import Nirum.Package.Metadata (Target)

targetTypesQ :: Q [Type]
targetTypesQ = do
    ClassI _ instances <- reify ''Target
    return [t | InstanceD _ _ (AppT _ t) _ <- instances]

targetProxiesQ :: Q [Exp]
targetProxiesQ = do
    targetTypes <- targetTypesQ
    sequenceQ [[e|(Proxy :: Proxy $(return t))|] | t <- targetTypes]

targetProxyMapQ :: Q Exp -> Q Exp
targetProxyMapQ funcQ = do
    targetProxies <- targetProxiesQ
    listE [ [e|(targetName $(return p), $funcQ $(return p))|]
          | p <- targetProxies
          ]
