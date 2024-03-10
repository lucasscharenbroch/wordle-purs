module GamePage where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Util (whenElem)

type State =
  { showInfo :: Boolean
  , showSettings :: Boolean
  , useFullDict :: Boolean
  }

data Action = HideInfoBox
            | HideSettingsBox
            | ShowInfoBox
            | ShowSettingsBox
            | TestAllWords
            | SetUseDictWords
            | SetUseWordleWords
            | ChangePageToSolver
            | ResetGame

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

container :: forall w. HH.HTML w Action
container =
  HH.div
    [HP.id "container"]
    [ header
    -- TODO
    ]

header :: forall w. HH.HTML w Action
header =
  HH.div
    [HP.id "header"]
    [ HH.div
        [HP.id "headerButtons1"]
        [ HH.i
            [ HP.style "font-size:24px"
            , HP.classes [HH.ClassName "fa"]
            ]
            [HH.text ""]
        , HH.i
            [ HP.style "font-size:24px"
            , HP.classes [HH.ClassName "fa"]
            , HP.id "infoButton"
            , HE.onClick \_ -> ShowInfoBox
            ]
            [HH.text ""]
        ]
    , HH.div
        [HP.id "title"]
        [HH.text "Wordle"]
    , HH.div
        [HP.id "headerButtons2"]
        [ HH.button
            [HE.onClick \_ -> ChangePageToSolver]
            [HH.text "Solver"]
        , HH.i
            [ HP.style "font-size:24px"
            , HP.classes [HH.ClassName "fa"]
            , HE.onClick \_ -> ResetGame
            ]
            [HH.text "↻"]
        , HH.i
            [ HP.style "font-size:24px"
            , HP.classes [HH.ClassName "fa"]
            , HE.onClick \_ -> ShowSettingsBox
            ]
            [HH.text "\xf013"]
        ]
    ]

infoBox :: forall w. HH.HTML w Action
infoBox =
  HH.div
    [ HP.id "infoBox"
    , HP.classes [HH.ClassName "messageBox"]
    ]
    [HH.h1_ [HH.text "Wordle"]
    , HH.text "Wordle is a fun word-guessing game. You have six attempts to guess a secret word."
    , HH.br_
    , HH.br_
    , HH.text "Enter words with the letter-buttons or keyboard, and press enter to guess."
    , HH.br_
    , HH.br_
    , HH.text "The box will be green if the letter and position is correct. The box will be yellow if the letter is in the word, but not in that position. The box will be gray if the letter does not exist in the word (beyond the boxes already colored)."
    , HH.br_
    , HH.br_
    , HH.text "Press \"solve\", and the solver will automatically make guesses for you."
    , HH.br_
    , HH.button
        [ HP.classes [ HH.ClassName "okButton" ]
        , HE.onClick \_ -> HideInfoBox
        ]
        [HH.text "OK"]
    ]

settingsBox :: State -> forall w. HH.HTML w Action
settingsBox state =
  HH.div
    [ HP.id "settingsBox"
    , HP.classes [HH.ClassName "messageBox"]
    ]
    [HH.h1_ [HH.text "Settings"]
    , HH.form_
        [ HH.label_
            [HH.text "Solver wordbase:"]
        , HH.br_
        , HH.input
            [ HP.id "dictionaryWords"
            , HP.type_ HP.InputRadio
            , HP.name "wordlist"
            , HP.checked state.useFullDict
            , HE.onClick \_ -> SetUseDictWords
            ]
        , HH.label
            [HP.for "dictionaryWords"]
            [HH.text "5-letter English word dictionary (21,000 words)"]
        , HH.br_
        , HH.input
            [ HP.id "wordleWords"
            , HP.type_ HP.InputRadio
            , HP.name "wordlist"
            , HP.checked $ not state.useFullDict
            , HE.onClick \_ -> SetUseWordleWords
            ]
        , HH.label
            [HP.for "wordleWords"]
            [HH.text "Official Wordle Word List (2,000 words)"]
        ]
    , HH.br_
    , HH.button
        [ HP.id "testButton"
        , HE.onClick \_ -> TestAllWords
        ]
        [HH.text "Test All Words in Word List"]
    , HH.text testStatus
    , HH.br_
    , HH.button
        [HP.classes [HH.ClassName "okButton"]
        , HE.onClick \_ -> HideSettingsBox
        ]
        [HH.text "OK"]
    ]
  where testStatus = "" -- TODO compute testStatus using state

render :: forall w. State -> HH.HTML w Action
render state =
    HH.main_
      [ container
      , whenElem state.showInfo (\_ -> infoBox)
      , whenElem state.showSettings (\_ -> settingsBox state)
      ]

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  HideInfoBox -> H.modify_ (\s -> s {showInfo = false})
  HideSettingsBox -> H.modify_ (\s -> s {showSettings = false})
  ShowInfoBox -> H.modify_ (\s -> s {showInfo = true})
  ShowSettingsBox -> H.modify_ (\s -> s {showSettings = true})
  SetUseDictWords -> H.modify_ (\s -> s {useFullDict = true})
  SetUseWordleWords -> H.modify_ (\s -> s {useFullDict = false})
  TestAllWords -> pure unit -- TODO
  ChangePageToSolver -> pure unit -- TODO
  ResetGame -> pure unit -- TODO

initialState :: Unit -> State
initialState _ =
  { showInfo: false
  , showSettings: false
  , useFullDict: true
  }

component :: forall output m t. H.Component t Unit output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
