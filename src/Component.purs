module Component where

import Prelude

import Halogen as H
import Halogen.HTML as HH

import Brainfuck (run) as B
import Brainfuck.Interp.Stream (defaultStream) as BIS
import Brainfuck.Interp.Log (noLog) as BIL
import Brainfuck.State (State(..)) as BS
import Brainfuck.Interp (InterpResult) as BI
import Brainfuck.Program (fromString) as BP
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


data Action
  = ChangeProgram String
  | ExecuteProgram


type State =
  { program :: String
  , result :: Maybe (BI.InterpResult Unit)
  , isExecutable :: Boolean
  }


initialState :: forall input. input -> State
initialState _ =
  { program: ""
  , result: Nothing
  , isExecutable: true
  }


component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }


render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ programArea state.isExecutable
    , case state.result of
        Just res ->
          interpResult res

        Nothing ->
          HH.div_ []
    ]


programArea :: forall w. Boolean -> HH.HTML w Action
programArea isExecutable=
  HH.div_
    [ HH.textarea [ HE.onValueChange ChangeProgram ]
    , HH.button
        [ HE.onClick (\_ -> ExecuteProgram)
        , HP.enabled isExecutable -- 追加
        ]
        [ HH.text "Execute" ]
    ]


interpResult :: forall w i. BI.InterpResult Unit -> HH.HTML w i
interpResult { result, state: BS.State { iptr, dptr, memory } } =
  let message =
        case result of
          Right _ ->
            "Succeed"

          Left err ->
            show err
  in
    HH.div_
      [ HH.div_ [ HH.text ("Result: " <> message) ]
      , HH.div_
          [ HH.ul_
              [ HH.li_ [ HH.text ("iptr: " <> show iptr) ]
              , HH.li_ [ HH.text ("dptr: " <> show dptr) ]
              , HH.li_ [ HH.text ("memory: " <> show memory) ]
              ]
          ]
      ]



handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction =
  case _ of
    ChangeProgram program ->
      H.modify_ _ { program = program }

    ExecuteProgram -> do
      { program } <- H.get
      H.modify_ _ { result = Nothing, isExecutable = false }
      res <- liftEffect $ B.run BIS.defaultStream BIL.noLog (BP.fromString program)
      H.modify_ _ { result = Just res, isExecutable = true }
