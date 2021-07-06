module Main where

import Prelude

import Brainfuck (run) as B
import Brainfuck.Program (fromString) as BP
import Effect (Effect)
import Effect.Aff (launchAff_)

import Brainfuck.Cli (cliLog, cliStream)
import Brainfuck.Cli.State (init) as CliState


main :: Effect Unit
main = do
  ref <- CliState.init
  launchAff_ $ B.run (cliStream ref) (cliLog ref) (BP.fromString "++++++++[>++++++++<-]>+.")
