module Brainfuck where

import Prelude

import Brainfuck.Env (getProgram, makeEnv)
import Brainfuck.Interp (Interp, InterpResult, runInterp)
import Brainfuck.Interp.Command (interpCommand)
import Brainfuck.Interp.Stream (Stream, defaultStream)
import Brainfuck.Interp.Util (incInstPtr)
import Brainfuck.Program (Program)
import Brainfuck.State (defaultState, readCommand)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)

import Brainfuck.Interp.Log (Log, logStart, logState, logCmd, logEnd, noLog, debugLog)


run :: forall m. Monad m => Stream m -> Log m -> Program -> m (InterpResult Unit)
run stream log program =
  runInterp (interpProgram stream log) (makeEnv program) defaultState


runDefault :: Program -> Effect (InterpResult Unit)
runDefault program = run defaultStream noLog program


runWithLog :: forall m. MonadEffect m => Stream m -> Program -> m Unit
runWithLog stream program = do
  res <- run stream debugLog program
  liftEffect $ log $ ("\n" <> show res)


interpProgram :: forall m. Monad m => Stream m -> Log m -> Interp m Unit
interpProgram stream log = do
  logStart log
  loop
  logEnd log
  where
    loop :: Interp m Unit
    loop = do
      program <- getProgram <$> ask
      state <- get
      logState log state
      case readCommand program state of
        Just cmd -> do
          logCmd log cmd
          interpCommand stream cmd

          incInstPtr
          loop

        Nothing ->
          pure unit

