module Page where

import Prelude
import State
import Wordle
import Util

import Data.Array (replicate, range, (!!), length)
import Data.Array as Array
import Data.Map (Map, empty)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe)
import Data.String as String
import Data.String.CodeUnits (charAt, fromCharArray, singleton, toCharArray)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Util (backspaceText, whenElem)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

render :: forall slots. State -> HH.HTML (H.ComponentSlot slots Aff Action) Action
render state =
    HH.main_
      [ container state
      , whenElem state.showInfo (\_ -> infoBox state.currentPage)
      , whenElem state.showSettings (\_ -> settingsBox state)
      ]

{- ALl Pages (Game and Solver) -}

container :: forall w. State -> HH.HTML w Action
container state =
  HH.div
    [HP.id "container"]
    [ header state.currentPage
    , mkBoard state.board
    , (case state.currentPage of
        (Game {keyboardState}) -> keyboard keyboardState
        (Solver _) -> solverButtons)
    ]

header :: forall w. Page -> HH.HTML w Action
header page =
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
            , HE.onClick \_ -> SetInfoBoxVis true
            ]
            [HH.text ""]
        ]
    , HH.div
        [HP.id "title"]
        [HH.text (case page of
                        Game _ -> "Wordle"
                        Solver _ -> "Solver")]
    , HH.div
        [HP.id "headerButtons2"]
        [ HH.button
            [HE.onClick \_ -> (case page of
                                Game _ -> ChangePageToSolver
                                Solver _ -> ChangePageToGame)]
            [HH.text (case page of
                        Game _ -> "Solver"
                        Solver _ -> "Game")]
        , HH.i
            [ HP.style "font-size:24px"
            , HP.classes [HH.ClassName "fa"]
            , HE.onClick \_ -> Reset
            ]
            [HH.text "↻"]
        , HH.i
            [ HP.style "font-size:24px"
            , HP.classes [HH.ClassName "fa"]
            , HE.onClick \_ -> SetSettingsBoxVis true
            ]
            [HH.text "\xf013"]
        ]
    ]

mkBoard :: forall w. Board -> HH.HTML w Action
mkBoard board =
  HH.div
    [HP.id "body"]
    (map mkRow board)
  where
    mkRow row =
      HH.div
        [HP.classes [HH.ClassName "row"]]
        (map mkCell row)
    mkCell {letter, color} =
      HH.div
        [ HP.classes [HH.ClassName "box"]
        , HP.style style
        ]
        [HH.text $ singleton letter]
      where
        style =
          case color of
            Green -> "background-color: rgb(106, 170, 100);"
            Yellow -> "background-color: rgb(201, 180, 88);"
            Gray -> "background-color: rgb(120, 124, 126);"
            None -> ""
          <> " "
          <> case letter of
                 ' ' -> ""
                 _ -> "border: 4px solid #696969;"

infoBox :: forall w. Page -> HH.HTML w Action
infoBox = case _ of
  Game _ -> wordleInfo
  Solver _ -> solverInfo

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
        , HE.onClick \_ -> SetSettingsBoxVis false
        ]
        [HH.text "OK"]
    ]
  where testStatus = "" -- TODO compute testStatus using state

{- Game-Page -}

keyboard :: forall w. KeyboardState -> HH.HTML w Action
keyboard kstate =
  HH.div
    [HP.id "keyboard"]
    keys
  where keys = map (toKeyboardRow <<< map charToKey <<< toCharArray) $ chars
        chars = ["QWERTYUIOP", "ASDFGHJKL", "eZXCVBNMb", "s"]
        mkKeyButton action extraProps =
          HH.button
            ([ HP.classes [HH.ClassName "key"]
            , HE.onClick \_ -> action
            ] <> extraProps)
        charToKey c = case c of
          'b' -> mkKeyButton (PressKeyButton KBack) [HP.id "back"] [backspaceText]
          'e' -> mkKeyButton PressEnter [HP.id "enter"] $ [HH.text "ENTER"]
          's' -> mkKeyButton SolveGame [HP.id "solve"] $ [HH.text "Solve"]
          _ -> mkKeyButton (PressKeyButton $ KLetter c) colorPropArr $ [HH.text $ singleton c]
            where colorPropArr = fromMaybe [] $ Array.singleton <<< colorToProp <$> Map.lookup c kstate
                  colorToProp = case _ of
                    Green -> HP.style $ "background-color: rgb(106, 170, 100);"
                    Yellow -> HP.style $ "background-color: rgb(201, 180, 88);"
                    Gray -> HP.style $ "background-color: rgb(120, 124, 126);"
                    None -> HP.style $ ""
        toKeyboardRow =
          HH.div
            [HP.classes [HH.ClassName "keyboardRow"]]

wordleInfo :: forall w. HH.HTML w Action
wordleInfo =
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
        , HE.onClick \_ -> SetInfoBoxVis false
        ]
        [HH.text "OK"]
    ]

{- Solver-Page -}

solverButtons :: forall w. HH.HTML w Action
solverButtons =
  HH.div
    [HP.id "buttons"]
    [ HH.div
        [HP.id "colorButtons"]
        [ HH.button
            [HE.onClick \_ -> PressColorKey Gray]
            [HH.text "Gray"]
        , HH.button
            [HE.onClick \_ -> PressColorKey Yellow]
            [HH.text "Yellow"]
        , HH.button
            [HE.onClick \_ -> PressColorKey Green]
            [HH.text "Green"]
        , HH.button
            [HE.onClick \_ -> PressColorKey None] -- None means backspace
            [backspaceText]
        ]
    , HH.div
        [HP.id "utilButtons"]
        [ HH.button
          [HE.onClick \_ -> GenerateGuess]
          [HH.text "Get Guess"]
        , HH.button
          [HE.onClick \_ -> RegenerateGuess]
          [HH.text "Invalid Word"]
        ]
    ]

solverInfo :: forall w. HH.HTML w Action
solverInfo =
  HH.div
    [ HP.id "infoBox"
    , HP.classes [HH.ClassName "messageBox"]
    ]
    [HH.h1_ [HH.text "Wordle Solver"]
    , HH.text "Use the solver tool to generate guesses for a Wordle game."
    , HH.br_
    , HH.br_
    , HH.text "Click \"Generate Guess\", and the solver will give you a word to guess."
    , HH.br_
    , HH.br_
    , HH.text $ "If a guess if invalid (which might be the case because the 5-letter-word " <>
                "dictionary contains over 200,000 words) click \"Invalid Word\", and the " <>
                "solver will disregaurd that guess and give you another one."
    , HH.br_
    , HH.br_
    , HH.text $ "Once you feed the guess into the game, tell the solver the colors of the " <>
                "letters by clicking the buttons labeled \"Gray\", \"Yellow\", and \"Green\"."
    , HH.br_
    , HH.button
        [ HP.classes [ HH.ClassName "okButton" ]
        , HE.onClick \_ -> SetInfoBoxVis false
        ]
        [HH.text "OK"]
    ]
