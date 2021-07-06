module Brainfuck.Cli where

import Prelude

import Brainfuck.Cli.State (State, init) as Cli
import Brainfuck.Cli.Util (clearLine)
import Brainfuck.Cli.Util as Cli
import Brainfuck.Env (getProgram)
import Brainfuck.Interp.Log (Log(..))
import Brainfuck.Interp.Stream (Stream(..))
import Brainfuck.Program (Program(..))
import Brainfuck.State (State(..))
import Control.Monad.Reader (ask)
import Data.Array (mapWithIndex) as Array
import Data.String (joinWith) as String
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Ref (Ref)


cliStream :: forall m. MonadAff m => Ref Cli.State -> Stream m
cliStream cliState = Stream { input, output }
  where
    input = pure 'N'

    output c = pure unit


cliLog :: forall m. MonadAff m => Ref Cli.State -> Log m
cliLog cliState = Log
  { onStart
  , onState
  , onCmd: \_ -> pure unit
  , onEnd
  }
  where
    onStart = do
       Cli.scrollDown 10
       Cli.up 10

    onState (State { iptr, dptr, memory }) = do
      program <- getProgram <$> ask
      Cli.printAt 0 cliState $ showProgram iptr program
      liftAff $ delay (Milliseconds 100.0)

    onEnd = pure unit


mapWithASpecialIndex :: forall a b. Int -> (a -> b) -> (a -> b) -> Array a -> Array b
mapWithASpecialIndex j fThen fElse =
  Array.mapWithIndex (\i x -> if i == j then fThen x else fElse x)


showProgram :: Int -> Program -> String
showProgram iptr (Program program) =
  String.joinWith "" $
    mapWithASpecialIndex iptr
      (Cli.highlight <<< show)
      show
      program

