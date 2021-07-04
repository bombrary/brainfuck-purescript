module Brainfuck.Env where

import Prelude

import Brainfuck.Program (Program)

newtype Env = Env
  { program :: Program
  }

instance Show Env where
  show (Env { program }) = show program


getProgram :: Env -> Program
getProgram (Env { program } ) = program


makeEnv :: Program -> Env
makeEnv program = Env
  { program
  }
