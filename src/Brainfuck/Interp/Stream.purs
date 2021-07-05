module Brainfuck.Interp.Stream where

import Prelude

import Brainfuck.Interp (Interp)

import Effect.Class (liftEffect)
import Effect.Console (log)

import Data.String.CodeUnits (singleton) as CodeUnits
import Node.Process (stdout)
import Node.Encoding (Encoding(UTF8))
import Node.Stream (writeString)


newtype Stream = Stream
  { input :: Interp Char
  , output ::Char -> Interp Unit
  }


read :: Stream -> Interp Char
read (Stream { input }) = input


write :: Char -> Stream -> Interp Unit
write c (Stream { output }) =
  output c


defaultStream :: Stream
defaultStream = Stream { input, output }
  where
    input = pure 'N' -- Not Implemented

    output c = liftEffect $ log $ show c


nodeStream :: Stream
nodeStream = Stream { input, output }
  where
    input = pure 'N' -- Not Implemented

    output c =
      void $ liftEffect $ writeString stdout UTF8 (CodeUnits.singleton c) (pure unit)
