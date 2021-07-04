module Brainfuck where

import Prelude

import Brainfuck.Env (getProgram, makeEnv)
import Brainfuck.Interp (Interp(..), InterpResult, runInterp)
import Brainfuck.Interp.Command (interpCommand)
import Brainfuck.Interp.Util (incInstPtr)
import Brainfuck.Program (Program(..))
import Brainfuck.State (defaultState, readCommand)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Data.Maybe (Maybe(..))
import Effect (Effect)


runDefault :: Program -> Effect (InterpResult Unit)
runDefault program = runInterp interpProgram (makeEnv program) defaultState


interpProgram :: Interp Unit
interpProgram = do
  program <- getProgram <$> ask
  state <- get
  case readCommand program state of
    Just cmd -> do
      interpCommand cmd

      incInstPtr
      interpProgram

    Nothing ->
      pure unit

