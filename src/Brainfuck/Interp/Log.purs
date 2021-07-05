module Brainfuck.Interp.Log where

import Prelude

import Brainfuck.Command (Command)
import Brainfuck.Interp (Interp)
import Brainfuck.State (State)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)


newtype Log m = Log
  { onStart :: Interp m Unit
  , onState :: State -> Interp m Unit
  , onCmd :: Command -> Interp m Unit
  , onEnd :: Interp m Unit
  }


logStart :: forall m. Log m -> Interp m Unit
logStart (Log { onStart }) = onStart


logState :: forall m. Log m -> State -> Interp m Unit
logState (Log { onState }) = onState


logCmd :: forall m. Log m -> Command -> Interp m Unit
logCmd (Log { onCmd }) = onCmd


logEnd :: forall m. Log m -> Interp m Unit
logEnd (Log { onEnd }) = onEnd


noLog :: forall m. Monad m => Log m
noLog = Log
  { onStart: pure unit
  , onState: \_ -> pure unit
  , onCmd: \_ -> pure unit
  , onEnd: pure unit
  }


debugLog :: forall m. MonadEffect m => Log m
debugLog = Log
  { onStart: liftEffect $ log "start"
  , onState: \s -> liftEffect $ log ("state:" <> show s)
  , onCmd: \c -> liftEffect $ log ("cmd: " <> show c)
  , onEnd: liftEffect $ log "end"
  }

