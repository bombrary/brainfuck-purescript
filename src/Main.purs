module Main where

import Prelude

import Brainfuck (runWithLog) as B
import Brainfuck.Interp.Stream (nodeStream) as BIS
import Brainfuck.Program (fromString) as BP
import Effect (Effect)
import Effect.Aff (launchAff_)

main :: Effect Unit
main =
  launchAff_ $ B.runWithLog BIS.nodeStream (BP.fromString ",>,>,<<+.>+.>+.")
