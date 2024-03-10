module Main where

import Prelude

import Data.Array (replicate)
import Data.String.CodeUnits (singleton)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Util (whenElem)

data Page = Game | Solver

type Board = Array (Array Cell) -- 6 x 5

data Color = Green | Yellow | Gray | None

type Cell =
  { color :: Color
  , letter :: Char
  }

type State =
  { showInfo :: Boolean
  , showSettings :: Boolean
  , useFullDict :: Boolean
  , currentPage :: Page
  , board :: Board
  }

data Action = HideInfoBox
            | HideSettingsBox
            | ShowInfoBox
            | ShowSettingsBox
            | TestAllWords
            | SetUseDictWords
            | SetUseWordleWords
            | ChangePageToSolver
            | ChangePageToGame
            | ResetGame

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

container :: forall w. State -> HH.HTML w Action
container state =
  HH.div
    [HP.id "container"]
    [ header state.currentPage
    , drawBoard state.board
    -- TODO
    ]

drawBoard :: forall w. Board -> HH.HTML w Action
drawBoard board =
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
            , HE.onClick \_ -> ShowInfoBox
            ]
            [HH.text ""]
        ]
    , HH.div
        [HP.id "title"]
        [HH.text (case page of
                        Game -> "Wordle"
                        Solver -> "Solver")]
    , HH.div
        [HP.id "headerButtons2"]
        [ HH.button
            [HE.onClick \_ -> (case page of
                                Game -> ChangePageToSolver
                                Solver -> ChangePageToGame)]
            [HH.text (case page of
                        Game -> "Solver"
                        Solver -> "Game")]
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
        , HE.onClick \_ -> HideInfoBox
        ]
        [HH.text "OK"]
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
        , HE.onClick \_ -> HideInfoBox
        ]
        [HH.text "OK"]
    ]

infoBox :: forall w. Page -> HH.HTML w Action
infoBox = case _ of
  Game -> wordleInfo
  Solver -> solverInfo

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
      [ container state
      , whenElem state.showInfo (\_ -> infoBox state.currentPage)
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
  ChangePageToSolver -> H.modify_ (\s -> s {currentPage = Solver})
  ChangePageToGame -> H.modify_ (\s -> s {currentPage = Game})
  TestAllWords -> pure unit -- TODO
  ResetGame -> pure unit -- TODO


defCell :: Cell
defCell = {color: None, letter: ' '}

defBoard :: Board
defBoard = replicate 6 (replicate 5 defCell)

initialState :: Unit -> State
initialState _ =
  { showInfo: false
  , showSettings: false
  , useFullDict: true
  , currentPage: Game
  , board: defBoard
  }

component :: forall output m t. H.Component t Unit output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
