module Brainfuck.CUI.Util where

import Prelude

import Brainfuck.CUI.State (dist, modifyY, State)
import Brainfuck.Interp (Interp)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, modify_, read) as Ref
import Node.Encoding (Encoding(UTF8))
import Node.Process (stdout)
import Node.Stream (writeString)
import Data.Array (replicate) as Array
import Data.String (joinWith) as String
import Node.ReadLine.Aff (question)
import Node.ReadLine (createConsoleInterface, noCompletion, close) as RL
import Data.String.CodeUnits (toChar, take) as CodeUnits
import Control.Monad.Error.Class (throwError)
import Brainfuck.Error (Error(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Data.Maybe (Maybe(..))


print :: forall m. MonadEffect m => String -> Interp m Unit
print str = void $ liftEffect $ writeString stdout UTF8 str (pure unit)


printAt :: forall m. MonadEffect m => Int -> Ref.Ref State -> String -> Interp m Unit
printAt y state str = do
  moveAt y state
  clearLine
  print str


moveAt :: forall m. MonadEffect m => Int -> Ref.Ref State -> Interp m Unit
moveAt y state = do
  dist <- liftEffect (dist y <$> Ref.read state)
  move dist state
  

move :: forall m. MonadEffect m => Int -> Ref.Ref State -> Interp m Unit
move x state = do
  liftEffect $ Ref.modify_ (modifyY (_ + x)) state
  if x > 0
    then down x
    else if x < 0
           then up (-x)
           else mostLeft


down :: forall m. MonadEffect m => Int -> Interp m Unit
down n = print ("\x01b[" <> show n <> "E")


up :: forall m. MonadEffect m => Int -> Interp m Unit
up n = print ("\x01b[" <> show n <> "F")


mostLeft :: forall m. MonadEffect m => Interp m Unit
mostLeft = print "\x01b[1G"


clearLine :: forall m. MonadEffect m => Interp m Unit
clearLine = print "\x01b[2K"


newLineTimes :: forall  m.  MonadEffect m => Int -> Interp m Unit
newLineTimes n = print $ String.joinWith "" $ Array.replicate n "\n"


highlight :: String -> String
highlight s = "\x01b[7m" <> s <> "\x01b[0m"


questionAndReadChar :: forall m. MonadAff m => Interp m Char
questionAndReadChar = do
  interface <- liftEffect $ RL.createConsoleInterface RL.noCompletion
  s <- liftAff $ question "input> " interface
  liftEffect $ RL.close interface
  case CodeUnits.toChar $ CodeUnits.take 1 s of
    Just c ->
      pure c

    Nothing ->
      throwError CharInputFailed

