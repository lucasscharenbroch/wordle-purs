module State where

import Prelude
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

{- Types -}

data Page = Game GameState
          | Solver SolverState

type GameState =
  { keyboardState :: KeyboardState
  , currentWord :: String
  , sentGuesses :: Array String
  , currentGuess :: String
  }

defGameState :: GameState
defGameState =
  { keyboardState: empty
  , currentWord: "GREAT" -- TODO generate word
  , sentGuesses: []
  , currentGuess: ""
  }
type SolverState =
  { guesses :: Array String
  }

defSolverState :: SolverState
defSolverState =
  { guesses: []
  }

data Key = KEnter
         | KBack
         | KLetter Char

type State =
  { showInfo :: Boolean
  , showSettings :: Boolean
  , useFullDict :: Boolean
  , currentPage :: Page
  , board :: Board
  }

initialState :: Unit -> State
initialState _ =
  { showInfo: false
  , showSettings: false
  , useFullDict: true
  , currentPage: Game defGameState
  , board: defBoard
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
            | SolveGame
            | InitKeybinds
            | HandleKeypress KeyboardEvent

{- All Pages -}

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
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
  SolveGame -> pure unit -- TODO
  InitKeybinds -> do
    document <- H.liftEffect $ Web.document =<< Web.window
    void <<< H.subscribe $
      eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map HandleKeypress <<< KE.fromEvent)
  HandleKeypress event -> res
    where k = KE.key event
          first = fromMaybe '_' <<< Array.head <<< toCharArray $ k
          res
            | KE.key event == "Enter" = handleAction $ PressKeyboardKey KEnter
            | KE.key event == "Backspace" = handleAction $ PressKeyboardKey KBack
            | String.length k == 1 && isLetter first = handleAction <<< PressKeyboardKey <<< KLetter <<< toUpper $ first
            | otherwise = pure unit

{- Game-Page -}

gameStateKeyPress :: GameState -> Key -> GameState
gameStateKeyPress state@{sentGuesses, currentGuess} (KLetter c)
  | length sentGuesses >= maxGuesses = state
  | String.length currentGuess < wordLength = state {currentGuess = currentGuess <> singleton c}
  | otherwise = state
gameStateKeyPress state@{currentGuess} KBack =
  case Array.unsnoc (toCharArray currentGuess) of
    Nothing -> state
    Just {init} -> state {currentGuess = fromCharArray init}
gameStateKeyPress state@{sentGuesses, currentGuess} KEnter
  | String.length currentGuess /= wordLength = state
  | otherwise = state {sentGuesses = sentGuesses <> [currentGuess], currentGuess = ""}

gameRenderBoard :: GameState -> Board
gameRenderBoard {currentWord, sentGuesses, currentGuess} =
  map (colorRow currentWord) sentGuesses
  <> [mkPaddedRow currentGuess]
  <> replicate (maxGuesses - length sentGuesses - 1) (replicate wordLength defCell)
  where mkPaddedRow s = map (\c -> {color: None, letter: c}) (toCharArray s) <> replicate (wordLength - String.length s) defCell

colorRow :: String -> String -> Array Cell
colorRow correctWord = gradeGuess (toCharArray correctWord) <<< toCharArray

{- Solver-Page -}

-- TODO
