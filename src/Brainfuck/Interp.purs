module Brainfuck.Interp where

import Prelude

import Brainfuck.Env (Env)
import Brainfuck.Error (Error)
import Brainfuck.State (State)
import Control.Monad.Except.Trans (class MonadThrow, ExceptT, runExceptT)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, runReaderT)
import Control.Monad.State.Trans (class MonadState, StateT, runStateT)
import Data.Either (Either)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)

import Effect.Aff.Class (class MonadAff)


newtype Interp m a = Interp (ReaderT Env (ExceptT Error (StateT State m)) a)


type InterpResult a =
  { result :: Either Error a
  , state :: State
  }


runInterp :: forall m a. Monad m => Interp m a -> Env -> State -> m (InterpResult a)
runInterp (Interp ip) env s = do
  Tuple result state <- runStateT (runExceptT (runReaderT ip env)) s
  pure { result, state }


derive newtype instance (Functor m) => Functor (Interp m)
derive newtype instance (Monad m) => Apply (Interp m)
derive newtype instance (Monad m) => Applicative (Interp m)
derive newtype instance (Monad m) => Bind (Interp m)
derive newtype instance (Monad m) => Monad (Interp m)
derive newtype instance (Monad m) => MonadState State (Interp m)
derive newtype instance (Monad m) => MonadAsk Env (Interp m)
derive newtype instance (Monad m) => MonadThrow Error (Interp m)
derive newtype instance (MonadEffect m) => MonadEffect (Interp m)
derive newtype instance (MonadAff m) => MonadAff (Interp m)
