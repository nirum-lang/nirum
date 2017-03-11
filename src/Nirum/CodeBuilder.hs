{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators #-}
module Nirum.CodeBuilder ( CodeBuilder
                         , nest
                         , runBuilder
                         , writeLine
                         ) where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Control.Monad.State (MonadState, State, runState, modify)
import Data.Functor (Functor)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy.Builder as B
import qualified Text.PrettyPrint as P
import Text.PrettyPrint (($+$))


newtype CodeBuilder a = CodeBuilder (State P.Doc a)
    deriving ( Applicative
             , Functor
             , Monad
             , MonadState P.Doc
             )

writeLine :: P.Doc -> CodeBuilder ()
writeLine code = modify $ \s -> s $+$ code

nest :: Integer -> CodeBuilder a -> CodeBuilder a
nest n code = do
    let CodeBuilder a = code
    let (ret , inner) = runState a P.empty
    writeLine $ P.nest (fromIntegral n) inner
    return ret

runBuilder :: CodeBuilder a -> (a, B.Builder)
runBuilder (CodeBuilder a) = (ret, rendered)
  where
    (ret, finalState) = runState a P.empty
    rendered = P.fullRender P.PageMode 80 1.5 concat' (B.singleton '\n') finalState
    concat' (P.Chr c) rest = B.singleton c <> rest
    concat' (P.Str s) rest = B.fromString s <> rest
    concat' (P.PStr s) rest = concat' (P.Str s) rest
