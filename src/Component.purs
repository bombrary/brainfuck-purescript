module Component where

import Prelude

import Halogen as H
import Halogen.HTML as HH

import Brainfuck (run) as B
import Brainfuck.Interp.Stream (Stream(..)) as BIS
import Brainfuck.Interp.Log (noLog) as BIL
import Brainfuck.State (State(..)) as BS
import Brainfuck.Interp (InterpResult) as BI
import Brainfuck.Program (fromString) as BP
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.String.CodeUnits (singleton) as CodeUnits
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Halogen.Subscription as HS

import Effect.Aff.AVar (AVar, put, take) as AVar
import Data.String.CodeUnits (toChar, take) as CodeUnits


data Action
  = Initialize
  | ChangeProgram String
  | ExecuteProgram
  | Output Char
  | RequestInput
  | ChangeInput String
  | ConfirmInput


type State =
  { program :: String
  , result :: Maybe (BI.InterpResult Unit)
  , isExecutable :: Boolean
  , streamMay :: Maybe (BIS.Stream Aff)
  , output :: String
  , avar :: AVar.AVar Char
  , input :: String
  , isInputEnabled :: Boolean
  }


initialState :: AVar.AVar Char -> State
initialState avar =
  { program: ""
  , result: Nothing
  , isExecutable: true
  , streamMay: Nothing
  , output: ""
  , avar
  , input: ""
  , isInputEnabled: false
  }


component :: forall query output m. MonadAff m => H.Component query (AVar.AVar Char) output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize -- 追加
        }
    }


render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ programArea state.isExecutable
    , if state.isInputEnabled 
        then inputArea
        else HH.div_ []
    , outputArea state.output
    , case state.result of
        Just res ->
          interpResult res

        Nothing ->
          HH.div_ []
    ]

inputArea :: forall w. HH.HTML w Action
inputArea =
  HH.div_
    [ HH.input
        [ HP.type_ HP.InputText
        , HE.onValueChange ChangeInput
        ]
    , HH.button
        [ HE.onClick (\_ -> ConfirmInput) ]
        [ HH.text "input" ]
    ]

outputArea :: forall w i. String -> HH.HTML w i
outputArea output =
  HH.div_
    [ HH.p_ [ HH.text ("Output: " <> output ) ] ]


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
    Initialize -> do
      { emitter, listener } <- liftEffect HS.create
      _ <- H.subscribe emitter
      H.modify_ \s -> s { streamMay = Just $ createStream listener s.avar }

    ChangeProgram program ->
      H.modify_ _ { program = program }

    ExecuteProgram -> do
      { program, streamMay } <- H.get
      for_ streamMay \s -> do
          H.modify_ _ { output = "" , result = Nothing, isExecutable = false }
          res <- liftAff $ B.run s BIL.noLog (BP.fromString program)
          H.modify_ _ { result = Just res, isExecutable = true }

    Output c ->
      H.modify_ (\s -> s { output = s.output <> (CodeUnits.singleton c) })

    RequestInput ->
      H.modify_ _ { isInputEnabled = true }

    ChangeInput input ->
      H.modify_ _ { input = input }

    ConfirmInput -> do
      { avar, input } <- H.get
      case CodeUnits.toChar $ CodeUnits.take 1 input of
        Just c -> do
          H.modify_ _ { input = "", isInputEnabled = false } -- (*1)
          liftAff $ AVar.put c avar -- (*2)

        Nothing ->
          pure unit


createStream :: HS.Listener Action -> AVar.AVar Char -> BIS.Stream Aff
createStream listener avar = BIS.Stream { input, output }
  where
    input = do
      liftEffect $ HS.notify listener RequestInput
      liftAff $ AVar.take avar

    output c =
      liftEffect $ HS.notify listener (Output c)
