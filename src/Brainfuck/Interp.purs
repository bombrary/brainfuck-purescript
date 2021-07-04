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


newtype Interp a = Interp (ReaderT Env (ExceptT Error (StateT State Effect)) a)


type InterpResult a =
  { result :: Either Error a
  , state :: State
  }


runInterp :: forall a. Interp a -> Env -> State -> Effect (InterpResult a)
runInterp (Interp ip) env s = do
  Tuple result state <- runStateT (runExceptT (runReaderT ip env)) s
  pure { result, state }


derive newtype instance Functor Interp
derive newtype instance Apply Interp
derive newtype instance Applicative Interp
derive newtype instance Bind Interp
derive newtype instance Monad Interp
derive newtype instance MonadState State Interp
derive newtype instance MonadAsk Env Interp
derive newtype instance MonadThrow Error Interp
derive newtype instance MonadEffect Interp
