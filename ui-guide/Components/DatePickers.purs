module UIGuide.Component.DatePickers where

import Prelude

import Data.Array as Data.Array
import Data.DateTime (DateTime(..))
import Data.Int as Data.Int
import Data.Maybe (Maybe(..))
import Data.Time as Data.Time
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Effect.Class.Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as Halogen.Svg.Attributes
import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Data.DateTime (unsafeMkDate, unsafeMkTime)
import Ocelot.Data.DateTime as Ocelot.Data.DateTime
import Ocelot.DatePicker as DatePicker
import Ocelot.DateTimePicker as DateTimePicker
import Ocelot.HTML.Properties (css)
import Ocelot.Slider as Ocelot.Slider
import Ocelot.Slider.Render as Ocelot.Slider.Render
import Ocelot.TimePicker as TimePicker
import Type.Proxy (Proxy(..))
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation


----------
-- Component Types

type State = 
  { disabled :: Boolean -- | Global enable/disable toggle
  , timeInterval :: TimePicker.Interval
  }

data Query a
data Action 
  = HandleTimePicker TimePicker.Output
  | HandleTimeSlider Ocelot.Slider.Output
  | Initialize
  | ToggleDisabled

----------
-- Child paths

type ChildSlot =
  ( datePicker :: DatePicker.Slot Int
  , timePicker :: TimePicker.Slot Int
  , dtp :: DateTimePicker.Slot Int
  , timeSlider :: Ocelot.Slider.Slot String
  )

_datePicker = Proxy :: Proxy "datePicker"
_timePicker = Proxy :: Proxy "timePicker"
_dtp = Proxy :: Proxy "dtp"
_timeSlider = Proxy :: Proxy "timeSlider"

----------
-- Component definition

component :: ∀ m
  . MonadAff m
 => H.Component Query Unit Void m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    }
  }
  where
    handleAction = case _ of  
      HandleTimePicker output -> case output of
        TimePicker.SelectionChanged mTime -> do
          H.liftEffect $ Effect.Class.Console.log $ "TimePicker SelectionChanged: " <> show mTime
        TimePicker.VisibilityChanged _ -> pure unit
        TimePicker.Searched query -> do
          H.liftEffect $ Effect.Class.Console.log $ "TimePicker Searched: " <> query
      HandleTimeSlider output -> case output of
        Ocelot.Slider.ValueChanged points -> case points of
          [ startPercent, endPercent ] -> do
            let
              getIndex :: { percent :: Number } -> Int
              getIndex = Data.Int.round <<< (_ * 0.24) <<< _.percent

              start :: Maybe Data.Time.Time
              start = Data.Array.index Ocelot.Data.DateTime.defaultTimeRange (getIndex startPercent)

              end :: Maybe Data.Time.Time
              end = Data.Array.index Ocelot.Data.DateTime.defaultTimeRange (getIndex endPercent)

            H.liftEffect <<< Effect.Class.Console.log $
              ( "start: " <> show (map Ocelot.Data.DateTime.formatTime start)
                  <> ", end: " <> show (map Ocelot.Data.DateTime.formatTime end)
              )
            H.modify_ _ { timeInterval = { start, end } }
          _ -> pure unit
      Initialize -> do
        void $ H.queryAll _timeSlider $ H.mkTell $ Ocelot.Slider.ReplaceThumbs [ { percent: 0.0 } , { percent: 100.0 }]
      ToggleDisabled -> do
        st <- H.modify \s -> s { disabled = not s.disabled }
        void $ H.tell _datePicker 0 $ DatePicker.SetDisabled st.disabled
        void $ H.tell _datePicker 1 $ DatePicker.SetDisabled st.disabled
        void $ H.tell _timePicker 0 $ TimePicker.SetDisabled st.disabled
        void $ H.tell _timePicker 1 $ TimePicker.SetDisabled st.disabled
        void $ H.tell _dtp 0 $ DateTimePicker.SetDisabled st.disabled
        void $ H.tell _dtp 1 $ DateTimePicker.SetDisabled st.disabled
        
    initialState :: Unit -> State
    initialState _ =
      { disabled: false
      , timeInterval:
          { start: Data.Array.head Ocelot.Data.DateTime.defaultTimeRange
          , end: Data.Array.last Ocelot.Data.DateTime.defaultTimeRange
          }
      }

    render
      :: State
      -> H.ComponentHTML Action ChildSlot m
    render = cnDocumentationBlocks

----------
-- HTML

content :: forall t1 t2. Array (HH.HTML t2 t1) -> HH.HTML t2 t1
content = Backdrop.content [ css "flex" ]

cnDocumentationBlocks :: ∀ m
  . MonadAff m
 => State -> H.ComponentHTML Action ChildSlot m
cnDocumentationBlocks state =
  HH.div_
    [ HH.h1
      [ css "font-normal mb-6" ] 
      [ HH.text "Date Pickers" ]
    , HH.h2
      [ css "font-medium text-grey-50 text-xl mb-6" ]
      [ HH.text "It's a date picker. Deal with it" ]
    , HH.div
      [ css "mb-6" ]
      [ HH.p
        [ css "inline mr-4" ]
        [ HH.text "You can toggle between enabled/disabled states" ]
      , Button.button
        [ HE.onClick \_ -> ToggleDisabled ]
        [ HH.text "Toggle" ]
      ]
    , HH.div_
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Standard" ]
            , FormField.fieldMid_
              { label: HH.text "Start"
              , helpText: [ HH.text "Choose a start date." ]
              , error: []
              , inputId: "start-date"
              }
              [ HH.slot_ _datePicker 0 DatePicker.component
                { targetDate: Nothing
                , selection: Nothing
                , disabled: false
                }
              ]
            , Format.caption_ [ HH.text "Standard Disabled" ]
            , FormField.fieldMid_
              { label: HH.text "Start"
              , helpText: [ HH.text "Choose a start date." ]
              , error: []
              , inputId: "start-date-disabled"
              }
              [ HH.slot_ _datePicker 2 DatePicker.component
                { targetDate: Nothing
                , selection: Nothing
                , disabled: true
                }
              ]
            ]
          ]
        , content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Hydrated" ]
            , FormField.fieldMid_
              { label: HH.text "End"
              , helpText: [ HH.text "Choose an end date." ]
              , error: []
              , inputId: "end-date"
              }
              [ HH.slot_ _datePicker 1 DatePicker.component
                { targetDate: Nothing
                , selection: Just $ unsafeMkDate 2019 1 1
                , disabled: false
                }
              ]
            , Format.caption_ [ HH.text "Hydrated Disabled" ]
            , FormField.fieldMid_
              { label: HH.text "End"
              , helpText: [ HH.text "Choose an end date." ]
              , error: []
              , inputId: "end-date-disabled"
              }
              [ HH.slot_ _datePicker 3 DatePicker.component
                { targetDate: Nothing
                , selection: Just $ unsafeMkDate 2019 1 1
                , disabled: true
                }
              ]
            ]
          ]
        ]
      ]
    , Documentation.block_
      { header: "Time Pickers"
      , subheader: "It's a time picker. Deal with it."
      }
      [ Halogen.HTML.div
        [ css "flex-1" ]
        [ Backdrop.backdrop_
          [ Backdrop.content_
            [ Card.card_
              [ Halogen.HTML.slot _timeSlider "Time Pickers"
                Ocelot.Slider.component
                timeSliderInput
                HandleTimeSlider
              ]
            ]
          ]
        , Backdrop.backdrop_
          [ content
            [ Card.card
              [ css "flex-1" ]
              [ Format.caption_ [ HH.text "Standard" ]
              , FormField.fieldMid_
                { label: HH.text "Start"
                , helpText: [ HH.text "Choose a start time." ]
                , error: []
                , inputId: "start-time"
                }
                [ HH.slot _timePicker 0 TimePicker.component
                  { disabled: false
                  , interval: Just state.timeInterval
                  , selection: Nothing
                  }
                  HandleTimePicker
                ]
              , Format.caption_ [ HH.text "Standard Disabled" ]
              , FormField.fieldMid_
                { label: HH.text "Start"
                , helpText: [ HH.text "Choose a start time." ]
                , error: []
                , inputId: "start-time-disabled"
                }
                [ HH.slot _timePicker 2 TimePicker.component
                  { disabled: true
                  , interval: Just state.timeInterval
                  , selection: Nothing
                  }
                  HandleTimePicker
                ]
              ]
            ]
          , content
            [ Card.card
              [ css "flex-1" ]
              [ Format.caption_ [ HH.text "Hydrated" ]
              , FormField.fieldMid_
                { label: HH.text "End"
                , helpText: [ HH.text "Choose an end time." ]
                , error: []
                , inputId: "end-time"
                }
                [ HH.slot _timePicker 1 TimePicker.component
                  { disabled: false
                  , interval: Just state.timeInterval
                  , selection: Just $ unsafeMkTime 12 0 0 0
                  }
                  HandleTimePicker
                ]
              , Format.caption_ [ HH.text "Hydrated Disabled" ]
              , FormField.fieldMid_
                { label: HH.text "End"
                , helpText: [ HH.text "Choose an end time." ]
                , error: []
                , inputId: "end-time-disabled"
                }
                [ HH.slot _timePicker 3 TimePicker.component
                  { disabled: true
                  , interval: Just state.timeInterval
                  , selection: Just $ unsafeMkTime 12 0 0 0
                  }
                  HandleTimePicker
                ]
              ]
            ]
          ]
        ]
      ]
    , Documentation.block_
      { header: "DateTime Pickers"
      , subheader: "We've combined them. Deal with it."
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Standard" ]
            , FormField.field_
              { label: HH.text "Start"
              , helpText: [ HH.text "Choose a start date and time." ]
              , error: []
              , inputId: "start"
              }
              [ HH.slot_ _dtp 0 DateTimePicker.component
                { targetDate: Nothing
                , selection: Nothing
                , disabled: false
                }
              ]
            , Format.caption_ [ HH.text "Standard Disabled" ]
            , FormField.field_
              { label: HH.text "Start"
              , helpText: [ HH.text "Choose a start date and time." ]
              , error: []
              , inputId: "start-disabled"
              }
              [ HH.slot_ _dtp 2 DateTimePicker.component
                { targetDate: Nothing
                , selection: Nothing
                , disabled: true
                }
              ]
            ]
          ]
        , content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Hydrated" ]
            , FormField.field_
              { label: HH.text "End"
              , helpText: [ HH.text "Choose an end date and time." ]
              , error: []
              , inputId: "end"
              }
              [ HH.slot_ _dtp 1 DateTimePicker.component
                { targetDate: Nothing
                , selection: Just $ DateTime (unsafeMkDate 2019 1 1) (unsafeMkTime 0 0 0 0)
                , disabled: false
                }
              ]
            , Format.caption_ [ HH.text "Hydrated Disabled" ]
            , FormField.field_
              { label: HH.text "End"
              , helpText: [ HH.text "Choose an end date and time." ]
              , error: []
              , inputId: "end-disabled"
              }
              [ HH.slot_ _dtp 3 DateTimePicker.component
                { targetDate: Nothing
                , selection: Just $ DateTime (unsafeMkDate 2019 1 1) (unsafeMkTime 0 0 0 0)
                , disabled: true
                }
              ]
            ]
          ]
        ]
      ]
    ]

timeSliderInput :: Ocelot.Slider.Input
timeSliderInput =
  { axis: Just axis
  , disabled: false
  , layout: config
  , marks: Just marks
  , minDistance: Nothing
  , renderIntervals: Data.Array.foldMap renderInterval
  }
  where
  axis :: Array { label :: String, percent :: Number }
  axis = Data.Array.mapWithIndex toLabel Ocelot.Data.DateTime.defaultTimeRange
    where
    toLabel :: Int -> Data.Time.Time -> { label :: String, percent :: Number }
    toLabel index time =
      { label: Ocelot.Data.DateTime.formatTime time
      , percent: (Data.Int.toNumber index) / 0.24
      }

  marks :: Array { percent :: Number }
  marks = Data.Array.mapWithIndex toMark Ocelot.Data.DateTime.defaultTimeRange
    where
    toMark :: Int -> Data.Time.Time -> { percent :: Number }
    toMark index _ = { percent: (Data.Int.toNumber index) / 0.24 }

  renderInterval :: Ocelot.Slider.Interval -> Array Halogen.HTML.PlainHTML
  renderInterval = case _ of
    Ocelot.Slider.StartToThumb _ -> []
    Ocelot.Slider.BetweenThumbs { left, right } ->
      [ Ocelot.Slider.Render.interval config
          { start: left, end: right }
          [ Halogen.Svg.Attributes.fill (pure (Halogen.Svg.Attributes.RGB 126 135 148)) ]
      ]
    Ocelot.Slider.ThumbToEnd _ -> []

  config :: Ocelot.Slider.Render.Config
  config =
    { axisHeight: 30.0
    , betweenThumbAndAxis: 30.0
    , betweenTopAndThumb: 20.0
    , frameWidth: { px: 1000.0 }
    , margin: 50.0
    , trackWidth: 1800.0
    , trackRadius: 5.0
    , thumbRadius: 20.0
    }
