module Component where

import Prelude

import Brainfuck (run) as B
import Brainfuck.Interp (InterpResult) as BI
import Brainfuck.Interp.Log (Log(..)) as BIL
import Brainfuck.Interp.Stream (Stream(..)) as BIS
import Brainfuck.Program (fromString, Program(..)) as BP
import Brainfuck.State (State(..)) as BS
import Data.Array (mapWithIndex) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton, toChar, take) as CodeUnits
import Data.Traversable (for_)
import Effect.Aff (Aff, delay, Milliseconds(..))
import Effect.Aff.AVar (AVar, put, take) as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Web.HTML.Common (ClassName(..))


data Action
  = Initialize
  | ChangeProgram String
  | ExecuteProgram
  | Output Char
  | RequestInput
  | ChangeInput String
  | ConfirmInput
  | RequestLogState BS.State


type State =
  { program :: String
  , result :: Maybe (BI.InterpResult Unit)
  , isExecutable :: Boolean
  , streamMay :: Maybe (BIS.Stream Aff)
  , output :: String
  , avar :: AVar.AVar Char
  , input :: String
  , isInputEnabled :: Boolean
  , logMay :: Maybe (BIL.Log Aff)
  , stateMay :: Maybe (BS.State)
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
  , logMay: Nothing
  , stateMay: Nothing
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
  HH.div [ HP.class_ (ClassName "app-outer") ]
    [ HH.div [ HP.class_ (ClassName "app") ]
      [ programArea state.isExecutable
      , stateArea (BP.fromString state.program) state.stateMay
      , inputArea state.isInputEnabled
      , outputArea state.output
      , interpResult state.result
      ]
    ]

stateArea :: forall w i. BP.Program -> Maybe BS.State -> HH.HTML w i
stateArea program stateMay =
  case stateMay of
    Just (BS.State { iptr, dptr, memory }) ->
      HH.div [ HP.class_ (ClassName "state-area") ]
        [ programLogArea iptr program
        , memoryLogArea dptr memory
        ]

    Nothing ->
      HH.div [ HP.class_ (ClassName "state-area") ]
        []


mapWithASpecialIndex :: forall a b. Int -> (a -> b) -> (a -> b) -> Array a -> Array b
mapWithASpecialIndex j fThen fElse =
  Array.mapWithIndex (\i x -> if i == j then fThen x else fElse x)


programLogArea :: forall w i. Int -> BP.Program -> HH.HTML w i
programLogArea iptr (BP.Program program) =
  HH.div [ HP.class_ (ClassName "program-log-area") ] $
    mapWithASpecialIndex iptr
      (\cmd -> HH.span [ HP.class_ (ClassName "program-log-area_log-highlight") ]
                       [ HH.text $ show cmd ])
      (\cmd -> HH.span [ HP.class_ (ClassName "program-log-area_log-usual") ]
                       [ HH.text $ show cmd ])
      program


memoryLogArea :: forall w i. Int -> Array Int -> HH.HTML w i
memoryLogArea dptr memory =
  HH.div [ HP.class_ (ClassName "memory-log-area") ] $
    mapWithASpecialIndex dptr
      (\dat -> HH.span [ HP.class_ (ClassName "memory-log-area_log-highlight") ]
                       [ HH.text $ show dat ])
      (\dat -> HH.span [ HP.class_ (ClassName "memory-log-area_log-usual")]
                       [ HH.text $ show dat ])
      memory


inputArea :: forall w. Boolean -> HH.HTML w Action
inputArea isInputEnabled =
  if isInputEnabled then
    HH.div [ HP.class_ (ClassName "input-area") ]
      [ HH.p [ HP.class_ (ClassName "input-area_label") ]
          [ HH.text "input> " ]
          , HH.div [ HP.class_ (ClassName "input-area_body")
          ]
          [ HH.input
              [ HP.type_ HP.InputText
              , HE.onValueChange ChangeInput
              ]
          , HH.button
              [ HE.onClick (\_ -> ConfirmInput) ]
              [ HH.text "submit" ]
          ]
      ]
  else
    HH.div [ HP.class_ (ClassName "input-area") ]
      [ HH.p [ HP.class_ (ClassName "input-area_label") ][ HH.text "input>"]
      , HH.div [ HP.class_ (ClassName "input-area_body") ] []
      ]


outputArea :: forall w i. String -> HH.HTML w i
outputArea output =
  HH.div [ HP.class_ (ClassName "output-area") ]
    [ HH.p [ HP.class_ (ClassName "output-area_label") ] [ HH.text "output>" ]
    , HH.p [ HP.class_ (ClassName "output-area_body") ] [ HH.text output ]
    ]


programArea :: forall w. Boolean -> HH.HTML w Action
programArea isExecutable=
  HH.div [ HP.class_ (ClassName "program-area") ]
  [ HH.input
      [ HE.onValueChange ChangeProgram
      , HP.type_ HP.InputText
      ]
    , HH.button
        [ HE.onClick (\_ -> ExecuteProgram)
        , HP.enabled isExecutable -- 追加
        ]
        [ HH.text "Execute" ]
    ]


interpResult :: forall w i. Maybe (BI.InterpResult Unit) -> HH.HTML w i
interpResult resultMay =
  case resultMay of
    Just { result } ->
      let message =
            case result of
              Right _ ->
                "Succeed"

              Left err ->
                show err
      in
        HH.div [ HP.class_ (ClassName "interp-result") ]
          [ HH.div_ [ HH.text ("Result> " <> message) ]
          ]
    Nothing ->
      HH.div [ HP.class_ (ClassName "interp-result") ] []



handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction =
  case _ of
    Initialize -> do
      { emitter, listener } <- liftEffect HS.create
      _ <- H.subscribe emitter
      H.modify_ \s ->
        s { streamMay = Just $ createStream listener s.avar
          , logMay = Just $ createLog listener
          }

    ChangeProgram program ->
      H.modify_ _ { program = program }

    ExecuteProgram -> do
      { program, streamMay, logMay } <- H.get
      for_ streamMay \s ->
        for_ logMay \l -> do
          H.modify_ _ { output = "" , result = Nothing, isExecutable = false }
          res <- liftAff $ B.run s l (BP.fromString program)
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

    RequestLogState state -> do
       H.modify_ _ { stateMay = Just state }


createStream :: HS.Listener Action -> AVar.AVar Char -> BIS.Stream Aff
createStream listener avar = BIS.Stream { input, output }
  where
    input = do
      liftEffect $ HS.notify listener RequestInput
      liftAff $ AVar.take avar

    output c =
      liftEffect $ HS.notify listener (Output c)


createLog :: HS.Listener Action -> BIL.Log Aff
createLog listener = BIL.Log
    { onStart: pure unit
    , onState
    , onCmd: \_ -> pure unit
    , onEnd: pure unit
    }
  where
    onState state = do
      liftEffect $ HS.notify listener (RequestLogState state)
      liftAff $ delay (Milliseconds 100.0)

