module Main where

import Prelude

import Data.Array (replicate, range, (!!))
import Data.Array as Array
import Data.Map (Map, empty)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe)
import Data.String as String
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Util (backspaceText, whenElem)

data Page = Game GameState
          | Solver SolverState

type GameState =
  { keyboardState :: KeyboardState
  , currentWord :: String
  , guesses :: Array String
  }
type SolverState =
  { guesses :: Array String
  }

type Board = Array (Array Cell) -- 6 x 5

data Color = Green | Yellow | Gray | None

data Key = KEnter
         | KBack
         | KSolve
         | KLetter Char

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

type KeyboardState = Map Char Color

data Action = SetInfoBoxVis Boolean
            | SetSettingsBoxVis Boolean
            | TestAllWords
            | SetUseDictWords
            | SetUseWordleWords
            | ChangePage Page
            | Reset
            | PressKeyboardKey Key
            | PressColorKey Color
            | GenerateGuess
            | RegenerateGuess

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

keyboard :: forall w. KeyboardState -> HH.HTML w Action
keyboard kstate =
  HH.div
    [HP.id "keyboard"]
    keys
  where keys = map (toKeyboardRow <<< map charToKey <<< toCharArray) $ chars
        chars = ["QWERTYUIOP", "ASDFGHJKL", "eZXCVBNMb", "s"]
        mkKeyButton k extraProps =
          HH.button
            ([ HP.classes [HH.ClassName "key"]
            , HE.onClick \_ -> PressKeyboardKey k
            ] <> extraProps)
        charToKey c = case c of
          'b' -> mkKeyButton KBack [HP.id "back"] [backspaceText]
          'e' -> mkKeyButton KEnter [HP.id "enter"] $ [HH.text "ENTER"]
          's' -> mkKeyButton KSolve [HP.id "solve"] $ [HH.text "Solve"]
          _ -> mkKeyButton (KLetter c) colorPropArr $ [HH.text $ singleton c]
            where colorPropArr = fromMaybe [] $ Array.singleton <<< colorToProp <$> Map.lookup c kstate
                  colorToProp = case _ of
                    Green -> HP.style $ "background-color: rgb(106, 170, 100);"
                    Yellow -> HP.style $ "background-color: rgb(201, 180, 88);"
                    Gray -> HP.style $ "background-color: rgb(120, 124, 126);"
                    None -> HP.style $ ""
        toKeyboardRow =
          HH.div
            [HP.classes [HH.ClassName "keyboardRow"]]

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

container :: forall w. State -> HH.HTML w Action
container state =
  HH.div
    [HP.id "container"]
    [ header state.currentPage
    , drawBoard state.board
    , (case state.currentPage of
        (Game {keyboardState}) -> keyboard keyboardState
        (Solver _) -> solverButtons)
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
                                Game _ -> ChangePage $ Solver defSolverState
                                Solver _ -> ChangePage $ Game defGameState)]
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

render :: forall w. State -> HH.HTML w Action
render state =
    HH.main_
      [ container state
      , whenElem state.showInfo (\_ -> infoBox state.currentPage)
      , whenElem state.showSettings (\_ -> settingsBox state)
      ]


wordLength :: Int
wordLength = 5

maxGuesses :: Int
maxGuesses = 6

gameStateKeyPress :: GameState -> Key -> GameState
gameStateKeyPress state@{guesses} (KLetter c) =
  case Array.unsnoc guesses of
    Nothing -> state {guesses = [singleton c]}
    Just {init, last}
      | String.length last == wordLength -> state {guesses = init <> [last]}
      | otherwise -> state {guesses = init <> [last <> singleton c]}
gameStateKeyPress state@{guesses} KBack =
  case Array.unsnoc guesses of
    Nothing -> state
    Just {init, last} ->
      case Array.unsnoc (toCharArray last) of
        Nothing -> state
        Just {init: sinit} -> state {guesses = init <> [fromCharArray sinit]}
gameStateKeyPress state _ = state -- TODO

gameRenderBoard :: GameState -> Board
gameRenderBoard state@{guesses} = map renderRow (range 0 5)
  where renderRow i = map (renderCell i) (range 0 4)
        renderCell i j =
          case guesses !! i >>= \s -> (toCharArray s) !! j of
            Nothing -> defCell
            Just c -> {color: None, letter: c}

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  SetInfoBoxVis isVis -> H.modify_ (\s -> s {showInfo = isVis})
  SetSettingsBoxVis isVis -> H.modify_ (\s -> s {showSettings = isVis})
  SetUseDictWords -> H.modify_ (\s -> s {useFullDict = true})
  SetUseWordleWords -> H.modify_ (\s -> s {useFullDict = false})
  ChangePage newPage -> H.modify_ (\s -> s {currentPage = newPage})
  PressKeyboardKey k -> H.modify_ (\s -> case s.currentPage of
                                   Game gState -> s { currentPage = Game gState'
                                                    , board = gameRenderBoard gState'
                                                    }
                                     where gState' = gameStateKeyPress gState k
                                   _ -> s)
  PressColorKey c -> pure unit -- TODO
  TestAllWords -> pure unit -- TODO
  Reset -> pure unit -- TODO
  GenerateGuess -> pure unit -- TODO
  RegenerateGuess -> pure unit -- TODO

defGameState :: GameState
defGameState =
  { keyboardState: empty
  , currentWord: "GREAT" -- TODO generate word
  , guesses: []
  }

defSolverState :: SolverState
defSolverState =
  { guesses: []
  }

defCell :: Cell
defCell = {color: None, letter: ' '}

defBoard :: Board
defBoard = replicate 6 (replicate 5 defCell)

initialState :: Unit -> State
initialState _ =
  { showInfo: false
  , showSettings: false
  , useFullDict: true
  , currentPage: Game defGameState
  , board: defBoard
  }

component :: forall output m t. H.Component t Unit output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
