module Main where

import Prelude

import Brainfuck (run) as B
import Brainfuck.Program (fromString) as BP
import Effect (Effect)
import Effect.Aff (launchAff_)

import Brainfuck.CUI (cuiLog, cuiStream)
import Brainfuck.CUI.State (init) as CUIState



main :: Effect Unit
main = do
  ref <- CUIState.init
  launchAff_ $ B.run (cuiStream ref) (cuiLog ref) (BP.fromString "++++++++[>++++++++>+<<-]>>++<+.+>.<.+>.<.")
