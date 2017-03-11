{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators #-}
module Nirum.CodeBuilder ( CodeBuilder
                         , IsCode
                         , nest
                         , runBuilder
                         , writeLine
                         ) where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Functor (Functor)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B


newtype CodeBuilder a = CodeBuilder (ReaderT BuildContext (Writer B.Builder) a)
    deriving ( Applicative
             , Functor
             , Monad
             , MonadReader BuildContext
             , MonadWriter B.Builder
             )

data BuildContext = BuildContext { indentSize :: Integer }

defaultContext :: BuildContext
defaultContext = BuildContext { indentSize = 0 }

class IsCode a where
    toCode :: a -> B.Builder

instance IsCode B.Builder where
    toCode = id

instance IsCode T.Text where
    toCode = B.fromText

writeLine :: (IsCode a) => a -> CodeBuilder ()
writeLine code = do
    BuildContext { indentSize = i } <- ask
    tell $ toCode $ T.replicate (fromIntegral i) " "
    tell $ toCode code
    tell "\n"

nest :: Integer -> CodeBuilder a -> CodeBuilder a
nest n = local (\BuildContext { indentSize = i } -> BuildContext { indentSize = i + n })

runBuilder :: CodeBuilder a -> (a, B.Builder)
runBuilder (CodeBuilder a) = runWriter (runReaderT a defaultContext)
