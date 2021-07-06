module Brainfuck.CUI.State where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref, new) as Ref

import Data.String.CodeUnits (singleton) as CodeUnits


newtype State = State
  { output :: String
  , y :: Int
  , outputLines :: Int
  }


init :: Effect (Ref.Ref State)
init =
  Ref.new $ State
    { output: ""
    , y: 0
    , outputLines: 0
    }


getOutputLines :: State -> Int
getOutputLines (State { outputLines }) = outputLines


incOntputLines :: State -> State
incOntputLines (State s@{ outputLines }) = State s { outputLines = outputLines + 1 }


getOutput :: State -> String
getOutput (State { output }) = output


appendOutput :: Char -> State -> State
appendOutput c (State s@{ output }) =
  State s { output = output <> (CodeUnits.singleton c) }


modifyY :: (Int -> Int) -> State -> State
modifyY f (State s@{ y }) = State s { y = f y }


setY :: Int -> State -> State
setY y = modifyY (\_ -> y)


dist :: Int -> State -> Int
dist y0 (State { y }) = y0 - y
