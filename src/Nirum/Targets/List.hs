{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Nirum.Targets.List ( proxyTypeMapQ
                          , proxyTypeQs
                          , targetTypesQ
                          , targetTypeQs
                          ) where

import Data.Proxy (Proxy (Proxy))
import Language.Haskell.TH (Exp, Q, Type)
import Language.Haskell.TH.Lib (listE)
import Language.Haskell.TH.Syntax (sequenceQ)

import Nirum.Targets.Python (Python)

targetTypeQs :: [Q Type]
targetTypeQs = [ [t|Python|]
               ] -- add target types here

targetTypesQ :: Q [Type]
targetTypesQ = sequenceQ targetTypeQs

proxyTypeQs :: [Q Exp]
proxyTypeQs = [[e|(Proxy :: Proxy $tQ)|] | tQ <- targetTypeQs]

proxyTypeMapQ :: Q Exp -> Q Exp
proxyTypeMapQ funcQ =
    listE [ [e|(targetName $(pQ), $funcQ $(pQ))|]
                         | pQ <- proxyTypeQs
                         ]
