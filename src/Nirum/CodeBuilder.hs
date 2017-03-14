{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators #-}
module Nirum.CodeBuilder ( CodeBuilder
                         , lookupType
                         , nest
                         , runBuilder
                         , writeLine
                         ) where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import qualified Control.Monad.State as ST
import Control.Monad.State (MonadState, State, state, runState)
import Data.Functor (Functor)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy.Builder as B
import qualified Text.PrettyPrint as P
import Text.PrettyPrint (($+$))

import Nirum.Constructs.Identifier (Identifier)
import Nirum.Constructs.ModulePath (ModulePath)
import qualified Nirum.Package as PK
import Nirum.Package (BoundModule (..), resolveBoundModule)
import Nirum.Package.Metadata (Package (..), Target (..))


newtype Target t => CodeBuilder t s a = CodeBuilder (State (BuildState t s) a)
    deriving ( Applicative
             , Functor
             , Monad
             )

data Target t => BuildState t s =
    BuildState { output :: P.Doc
               , boundModule :: BoundModule t
               , innerState :: s
               }

instance Target t => MonadState s (CodeBuilder t s) where
    state f = do
        st <- get'
        let (a, s) = f (innerState st)
        put' $ st { innerState = s }
        return a

get' :: Target t => CodeBuilder t s (BuildState t s)
get' = CodeBuilder ST.get

put' :: Target t => BuildState t s -> CodeBuilder t s ()
put' = CodeBuilder . ST.put

modify' :: Target t => (BuildState t s -> BuildState t s) -> CodeBuilder t s ()
modify' = CodeBuilder . ST.modify

writeLine :: Target t => P.Doc -> CodeBuilder t s ()
writeLine code = modify' $ \s -> s { output = output s $+$ code }

nest :: Target t => Integer -> CodeBuilder t s a -> CodeBuilder t s a
nest n code = do
    st <- get'
    let st' = st { output = P.empty }
    put' st'
    ret <- code
    after <- get'
    modify' $ \s -> s { output = output st $+$ P.nest (fromIntegral n) (output after) }
    return ret

lookupType :: Target t => Identifier -> CodeBuilder t s PK.TypeLookup
lookupType identifier = do
    m <- fmap boundModule get'
    return $ PK.lookupType identifier m

runBuilder :: Target t => Package t -> ModulePath -> s -> CodeBuilder t s a -> (a, B.Builder)
runBuilder package modPath st (CodeBuilder a) = (ret, rendered)
  where
    initialState = BuildState { output = P.empty
                              , boundModule = fromMaybe (error "asdf") (resolveBoundModule modPath package)
                              , innerState = st
                              }
    (ret, finalState) = runState a initialState
    rendered = P.fullRender P.PageMode 80 1.5 concat' (B.singleton '\n') (output finalState)
    concat' (P.Chr c) rest = B.singleton c <> rest
    concat' (P.Str s) rest = B.fromString s <> rest
    concat' (P.PStr s) rest = concat' (P.Str s) rest
