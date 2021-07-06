module Brainfuck.CUI where

import Prelude

import Brainfuck.CUI.Util (clearLine, highlight, moveAt, newLineTimes, printAt, questionAndReadChar, up) as CUI
import Brainfuck.CUI.State (State, appendOutput, getOutput, incOntputLines, getOutputLines) as CUI
import Brainfuck.Env (getProgram)
import Brainfuck.Interp.Log (Log(..))
import Brainfuck.Interp.Stream (Stream(..))
import Brainfuck.Program (Program(..))
import Brainfuck.State (State(..))
import Control.Monad.Reader (ask)
import Data.Array (mapWithIndex) as Array
import Data.String (joinWith) as String
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref (modify_, read) as Ref


cuiStream :: forall m. MonadAff m => Ref CUI.State -> Stream m
cuiStream cuiState = Stream { input, output }
  where
    input = do
      CUI.moveAt 2 cuiState
      s <- CUI.questionAndReadChar
      CUI.up 1
      CUI.clearLine
      pure s

    output c = do
      when (c == '\n') do
         liftEffect $ Ref.modify_ CUI.incOntputLines cuiState
      liftEffect $ Ref.modify_ (CUI.appendOutput c) cuiState
      st <- liftEffect $ Ref.read cuiState
      CUI.printAt 2 cuiState $ CUI.getOutput st
      CUI.up $ CUI.getOutputLines st


cuiLog :: forall m. MonadAff m => Ref CUI.State -> Log m
cuiLog cuiState = Log
  { onStart
  , onState
  , onCmd: \_ -> pure unit
  , onEnd
  }
  where
    onStart = do
       CUI.newLineTimes 4
       CUI.up 4

    onState (State { iptr, dptr, memory }) = do
      program <- getProgram <$> ask
      CUI.printAt 0 cuiState $ showProgram iptr program
      CUI.printAt 1 cuiState $ showMemory dptr memory
      liftAff $ delay (Milliseconds 100.0)

    onEnd = do
      st <- liftEffect $ Ref.read cuiState
      CUI.moveAt (3 + CUI.getOutputLines st) cuiState


mapWithASpecialIndex :: forall a b. Int -> (a -> b) -> (a -> b) -> Array a -> Array b
mapWithASpecialIndex j fThen fElse =
  Array.mapWithIndex (\i x -> if i == j then fThen x else fElse x)


showProgram :: Int -> Program -> String
showProgram iptr (Program program) =
  String.joinWith "" $
    mapWithASpecialIndex iptr
      (CUI.highlight <<< show)
      show
      program


showMemory :: Int -> Array Int -> String
showMemory dptr memory =
  String.joinWith " " $
    mapWithASpecialIndex dptr
      (CUI.highlight <<< show)
      show
      memory
