module Brainfuck.Interp.Stream where

import Prelude

import Brainfuck.Interp (Interp)

import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Process (stdout)
import Node.Encoding (Encoding(UTF8))
import Node.Stream (writeString)
import Effect (Effect)

import Brainfuck.Error (Error(..))
import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton, take, toChar) as CodeUnits
import Effect.Aff.Class (liftAff)
import Effect.Aff (Aff)
import Node.ReadLine (createConsoleInterface, noCompletion, close) as RL
import Node.ReadLine.Aff (question)


newtype Stream m = Stream
  { input :: Interp m Char
  , output :: Char -> Interp m Unit
  }


read :: forall m. Stream m -> Interp m Char
read (Stream { input }) = input


write :: forall m. Char -> Stream m -> Interp m Unit
write c (Stream { output }) =
  output c


defaultStream :: Stream Effect
defaultStream = Stream { input, output }
  where
    input = pure 'N' -- Not Implemented

    output c = liftEffect $ log $ show c


nodeStream :: Stream Aff
nodeStream = Stream { input, output }
  where
    input = do 
      interface <- liftEffect $ RL.createConsoleInterface RL.noCompletion
      s <- liftAff $ question "input> " interface
      liftEffect $ RL.close interface
      case CodeUnits.toChar $ CodeUnits.take 1 s of
        Just c ->
          pure c

        Nothing ->
          throwError CharInputFailed

    output c =
      void $ liftEffect $ writeString stdout UTF8 (CodeUnits.singleton c) (pure unit)
