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
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton, take, toChar) as CodeUnits
import Effect.Exception (Error) as E
import Effect.Aff.Class (liftAff)
import Effect.Aff (Aff, Canceler, nonCanceler, makeAff)
import Node.ReadLine (createConsoleInterface, noCompletion, close, question, Interface) as RL


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
      s <- liftAff $ questionAff "input> " interface
      liftEffect $ RL.close interface
      case CodeUnits.toChar $ CodeUnits.take 1 s of
        Just c ->
          pure c

        Nothing ->
          throwError CharInputFailed

    output c =
      void $ liftEffect $ writeString stdout UTF8 (CodeUnits.singleton c) (pure unit)


questionAff :: String -> RL.Interface -> Aff String
questionAff q interface = makeAff go
  where
    go :: (Either E.Error String -> Effect Unit) -> Effect Canceler
    go handler = do
      RL.question q (handler <<< Right) interface
      pure nonCanceler
