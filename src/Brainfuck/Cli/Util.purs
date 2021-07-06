module Brainfuck.Cli.Util where

import Prelude

import Brainfuck.Cli.State (dist, setY, modifyY, State)
import Brainfuck.Interp (Interp)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, modify_, read) as Ref
import Node.Encoding (Encoding(UTF8))
import Node.Process (stdout)
import Node.Stream (writeString)

import Effect.Console (log)


print :: forall m. MonadEffect m => String -> Interp m Unit
print str = void $ liftEffect $ writeString stdout UTF8 str (pure unit)


printAt :: forall m. MonadEffect m => Int -> Ref.Ref State -> String -> Interp m Unit
printAt y state str = do
  dist <- liftEffect (dist y <$> Ref.read state)
  move dist state
  print str
  

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


scrollDown :: forall  m.  MonadEffect m => Int -> Interp m Unit
scrollDown n = print ("\x01b[" <> show n <> "S")


highlight :: String -> String
highlight s = "\x01b[7m" <> s <> "\x01b[0m"
