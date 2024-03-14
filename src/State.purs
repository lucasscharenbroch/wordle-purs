module State where

import Prelude
import Util
import Wordle

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
import Web.HTML.Window (document, alert) as Web
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
  , isWin :: Boolean
  }

defGameState :: GameState
defGameState =
  { keyboardState: empty
  , currentWord: "GREAT" -- TODO generate word
  , sentGuesses: []
  , currentGuess: ""
  , isWin: false
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
            | PressKeyButton Key
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
  PressKeyButton k -> pressKeyButton k
  InitKeybinds -> initKeybinds
  HandleKeypress event -> handleKeypressEvent event
  PressColorKey c -> pure unit -- TODO
  TestAllWords -> pure unit -- TODO
  Reset -> pure unit -- TODO
  GenerateGuess -> pure unit -- TODO
  RegenerateGuess -> pure unit -- TODO
  SolveGame -> pure unit -- TODO

pressKeyButton :: forall o. Key -> H.HalogenM State Action () o Aff Unit
pressKeyButton k =
  do
    s <- H.get
    case s.currentPage of
      Game gState ->
        let wasWin = gameIsWin gState
            gState' = gameStateKeyPress gState k
            isWin = gameIsWin gState'
            justWon = not wasWin && isWin
            gState'' = gState' {isWin = wasWin || isWin}
        in do H.put $ s { currentPage = Game gState''
                        , board = gameRenderBoard gState''
                        }
              if justWon
              then H.liftEffect $ Web.alert (successMessage $ length gState''.sentGuesses) =<< Web.window
              else pure unit
      _ -> pure unit

gameIsWin :: GameState -> Boolean
gameIsWin {sentGuesses, currentWord} = case Array.last sentGuesses of
  Nothing -> false
  Just s -> allGreen (gradeGuess_ currentWord s)
    where allGreen = Array.all (\x -> isGreen x.color)
          isGreen Green = true
          isGreen _ = false

successMessage :: Int -> String
successMessage 1 = "First try? No way!"
successMessage 2 = "Amazing! You're incredible!"
successMessage 3 = "Wonderful! Very impressive."
successMessage 4 = "Nice job! :)"
successMessage _ = "Nice job! Never worried."

initKeybinds :: forall o. H.HalogenM State Action () o Aff Unit
initKeybinds = do
  document <- H.liftEffect $ Web.document =<< Web.window
  void <<< H.subscribe $
    eventListener
      KET.keyup
      (HTMLDocument.toEventTarget document)
      (map HandleKeypress <<< KE.fromEvent)

handleKeypressEvent :: forall o. KeyboardEvent -> H.HalogenM State Action () o Aff Unit
handleKeypressEvent event = res
  where
    k = KE.key event
    first = fromMaybe '_' <<< Array.head <<< toCharArray $ k
    res
      | KE.key event == "Enter" = handleAction $ PressKeyButton KEnter
      | KE.key event == "Backspace" = handleAction $ PressKeyButton KBack
      | String.length k == 1 && isLetter first = handleAction <<< PressKeyButton <<< KLetter <<< toUpper $ first
      | otherwise = pure unit

{- Game-Page -}

gameStateKeyPress :: GameState -> Key -> GameState
gameStateKeyPress state@{sentGuesses, currentGuess, isWin} (KLetter c)
  | isWin = state
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
