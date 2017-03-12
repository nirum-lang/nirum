{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators #-}
module Nirum.CodeBuilder ( CodeBuilder
                         , nest
                         , runBuilder
                         , writeLine
                         ) where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import qualified Control.Monad.State as ST
import Control.Monad.State (State, runState)
import Data.Functor (Functor)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy.Builder as B
import qualified Text.PrettyPrint as P
import Text.PrettyPrint (($+$))

import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Package (BoundModule (..), resolveBoundModule)
import Nirum.Package.Metadata (Package (..), Target (..))


newtype Target t => CodeBuilder t a = CodeBuilder (State (BuildState t) a)
    deriving ( Applicative
             , Functor
             , Monad
             )

data Target t => BuildState t =
    BuildState { output :: P.Doc
               , boundModule :: BoundModule t
               }

get :: Target t => CodeBuilder t (BuildState t)
get = CodeBuilder ST.get

put :: Target t => BuildState t -> CodeBuilder t ()
put = CodeBuilder . ST.put

modify :: Target t => (BuildState t -> BuildState t) -> CodeBuilder t ()
modify = CodeBuilder . ST.modify

writeLine :: Target t => P.Doc -> CodeBuilder t ()
writeLine code = modify $ \s -> s { output = output s $+$ code }

nest :: Target t => Integer -> CodeBuilder t a -> CodeBuilder t a
nest n code = do
    st <- get
    let st' = st { output = P.empty }
    put st'
    ret <- code
    after <- get
    modify $ \s -> s { output = output st $+$ P.nest (fromIntegral n) (output after) }
    return ret

runBuilder :: Target t => Package t -> ModulePath -> CodeBuilder t a -> (a, B.Builder)
runBuilder package modPath (CodeBuilder a) = (ret, rendered)
  where
    initialState = BuildState { output = P.empty
                              , boundModule = fromMaybe (error "asdf") (resolveBoundModule modPath package)
                              }
    (ret, finalState) = runState a initialState
    rendered = P.fullRender P.PageMode 80 1.5 concat' (B.singleton '\n') (output finalState)
    concat' (P.Chr c) rest = B.singleton c <> rest
    concat' (P.Str s) rest = B.fromString s <> rest
    concat' (P.PStr s) rest = concat' (P.Str s) rest
