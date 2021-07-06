module Node.ReadLine.Aff where


import Prelude
import Effect.Aff (Aff, Canceler, nonCanceler, makeAff)
import Node.ReadLine (question, Interface) as RL
import Data.Either (Either(..))
import Effect.Exception (Error) as E
import Effect (Effect)


question :: String -> RL.Interface -> Aff String
question q interface = makeAff go
  where
    go :: (Either E.Error String -> Effect Unit) -> Effect Canceler
    go handler = do
      RL.question q (handler <<< Right) interface
      pure nonCanceler
