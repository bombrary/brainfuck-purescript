module Brainfuck.State where

import Prelude

import Brainfuck.Command (Command)
import Brainfuck.Program (Program, readAt)
import Data.Array (modifyAt, (!!))
import Data.Array (replicate) as Array
import Data.Maybe (Maybe)


newtype State = State
  { dptr :: Int
  , iptr :: Int
  , memory :: Array Int
  }

derive newtype instance Show State


modifyDataPtr :: (Int -> Int) -> State -> State
modifyDataPtr f (State s@{ dptr }) = State s { dptr = f dptr }


readData :: State -> Maybe Int
readData (State { memory, dptr }) = memory !! dptr


modifyData :: (Int -> Int) -> State -> Maybe State
modifyData f (State s@{ memory, dptr }) =
  map
    (\newMem -> State s { memory = newMem })
    (modifyAt dptr f memory)


modifyInstPtr :: (Int -> Int) -> State -> State
modifyInstPtr f (State s@{ iptr }) = State s { iptr = f iptr }


readCommand :: Program -> State -> Maybe Command
readCommand p (State { iptr }) = readAt iptr p


defaultState :: State
defaultState = State
  { iptr: 0
  , dptr: 0
  , memory: Array.replicate 10 0
  }
