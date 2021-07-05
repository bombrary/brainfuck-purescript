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


run :: Stream -> Program -> Effect (InterpResult Unit)
run stream program = runInterp (interpProgram stream) (makeEnv program) defaultState


runDefault :: Program -> Effect (InterpResult Unit)
runDefault program = run defaultStream program


interpProgram :: Stream -> Interp Unit
interpProgram stream = do
  program <- getProgram <$> ask
  state <- get
  case readCommand program state of
    Just cmd -> do
      interpCommand stream cmd

      incInstPtr
      interpProgram stream

    Nothing ->
      pure unit

