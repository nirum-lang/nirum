{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, TypeOperators #-}
-- | The 'CodeBuilder' monad.
module Nirum.CodeBuilder (
    -- * The CodeBuilder monad
    CodeBuilder,
    runBuilder,
    -- * Builder operations
    writeLine,
    nest,
    lookupType,
    -- * Examples
    -- $examples
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

-- | A code builder monad parameterized by:
--
--     * @t@ - The build target
--     * @s@ - The state
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

modify' :: Target t
        => (BuildState t s -> BuildState t s)
        -> CodeBuilder t s ()
modify' = CodeBuilder . ST.modify

-- | Put the line below the builder output.
writeLine :: Target t
          => P.Doc               -- ^ The line to append
          -> CodeBuilder t s ()
writeLine code = modify' $ \s -> s { output = output s $+$ code }

-- | Nest (or indent) an output of inner builder computation by a given number
-- of positions.
nest :: Target t
     => Integer            -- ^ indentation size (may also be negative)
     -> CodeBuilder t s a  -- ^ inner builder computation to generate the
                           --   nested document
     -> CodeBuilder t s a
nest n code = do
    st <- get'
    let st' = st { output = P.empty }
    put' st'
    ret <- code
    after <- get'
    modify' $ \s -> s { output = output st $+$ P.nest (fromIntegral n) (output after) }
    return ret

-- | Look up the actual type by the name from the context of the builder
-- computation.
lookupType :: Target t
           => Identifier                     -- ^ name of the type to find
           -> CodeBuilder t s PK.TypeLookup
lookupType identifier = do
    m <- fmap boundModule get'
    return $ PK.lookupType identifier m

-- | Execute the builder computation and retrive output.
runBuilder :: Target t
           => Package t
           -> ModulePath
           -> s                  -- ^ initial state
           -> CodeBuilder t s a  -- ^ code builder computation to execute
           -> (a, B.Builder)     -- ^ return value and build result
runBuilder package modPath st (CodeBuilder a) = (ret, rendered)
  where
    mod' = fromMaybe (error "asdf") (resolveBoundModule modPath package)
    initialState = BuildState { output = P.empty
                              , boundModule = mod'
                              , innerState = st
                              }
    (ret, finalState) = runState a initialState
    rendered = P.fullRender P.PageMode 80 1.5 concat' (B.singleton '\n') (output finalState)
    concat' (P.Chr c) rest = B.singleton c <> rest
    concat' (P.Str s) rest = B.fromString s <> rest
    concat' (P.PStr s) rest = concat' (P.Str s) rest

{- $examples

> import Text.PrettyPrint (colon, empty, parens, quotes, (<>), (<+>))
>
> hello = do
>     writeLine $ "def" <+> "hello" <> parens empty <> colon
>     nest 4 $ do
>         writeLine $ "print" <> parens (quotes "Hello, world!")
>         writeLine $ "return" <+> "42"

-}
