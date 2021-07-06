module Main where

import Prelude

import Brainfuck (run) as B
import Brainfuck.CUI (cuiLog, cuiStream)
import Brainfuck.CUI.State (init) as CUIState
import Brainfuck.Program (fromString, Program) as BP
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Node.ReadLine (createConsoleInterface, noCompletion, close) as RL
import Node.ReadLine.Aff (question) as RL

main :: Effect Unit
main = do
  ref <- CUIState.init
  launchAff_ do
    program <- inputProgram
    B.run (cuiStream ref) (cuiLog ref) program


inputProgram :: Aff BP.Program
inputProgram = do
  interface <- liftEffect $ RL.createConsoleInterface RL.noCompletion
  s <- RL.question "program> " interface
  liftEffect $ RL.close interface
  pure (BP.fromString s)
