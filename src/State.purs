module State where

import Prelude
import Util
import Wordle

import Data.Array (replicate, range, (!!), length, elem, foldl)
import Data.Array as Array
import Data.Map (Map, empty)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe)
import Data.String as String
import Data.String.CodeUnits (charAt, fromCharArray, singleton, toCharArray)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Random (randomInt)
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
import WordList (dictWordList, wordleWordList)
import Data.Tuple

{- Types -}

data Page = Game GameState
          | Solver SolverState

type GameState =
  { keyboard :: KeyboardState
  , currentWord :: String
  , sentGuesses :: Array String
  , currentGuess :: String
  , isWin :: Boolean
  , board :: Board
  }

mkGameSeed :: Array String -> Effect Int
mkGameSeed wordList = randomInt 0 (length wordList - 1)

-- used in initial call of mkGameState on start-up
mkDefGameSeed :: Effect Int
mkDefGameSeed = mkGameSeed dictWordList

mkGameState :: Int -> Array String -> GameState
mkGameState seed wordList =
  { keyboard: empty
  , currentWord: case wordList !! seed of
                   Just s -> s
                   _ -> "BUGGY" -- shouldn't happen
  , sentGuesses: []
  , currentGuess: ""
  , isWin: false
  , board: defBoard
  }

type SolverState =
  { guesses :: Array String
  , board :: Board
  , sentColorings :: Array (Array Color)
  , currentColoring :: Array Color
  }

defSolverState :: SolverState
defSolverState =
  { guesses: []
  , board: defBoard
  , sentColorings: []
  , currentColoring: []
  }

data Key = KLetter Char
         | KBack

type State =
  { showInfo :: Boolean
  , showSettings :: Boolean
  , useFullDict :: Boolean
  , currentPage :: Page
  }

defState :: State
defState =
  { showInfo: false
  , showSettings: false
  , useFullDict: true
  , currentPage: Solver defSolverState
  }

initialState :: Int -> State
initialState  seed = defState {currentPage = Game $ mkGameState seed dictWordList}

type KeyboardState = Map Char Color

data Action = SetInfoBoxVis Boolean
            | SetSettingsBoxVis Boolean
            | TestAllWords
            | SetUseDictWords
            | SetUseWordleWords
            | ChangePageToGame
            | ChangePageToSolver
            | Reset
            | PressKeyButton Key
            | PressColorKey Color
            | GenerateGuess
            | RegenerateGuess
            | SolveGame
            | InitKeybinds
            | HandleKeypress KeyboardEvent
            | PressEnter

{- All Pages -}

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  SetInfoBoxVis isVis -> H.modify_ (\s -> s {showInfo = isVis})
  SetSettingsBoxVis isVis -> H.modify_ (\s -> s {showSettings = isVis})
  SetUseDictWords -> H.modify_ (\s -> s {useFullDict = true})
  SetUseWordleWords -> H.modify_ (\s -> s {useFullDict = false})
  ChangePageToSolver -> H.modify_ (\s -> s {currentPage = Solver defSolverState})
  ChangePageToGame -> mkGameStateM >>= \gs -> H.modify_ (\s -> s {currentPage = Game gs})
  PressKeyButton k -> pressKeyButton k
  InitKeybinds -> initKeybinds
  HandleKeypress event -> handleKeypressEvent event
  PressEnter -> pressEnter
  Reset -> resetState
  PressColorKey c -> pure unit -- TODO
  TestAllWords -> pure unit -- TODO
  GenerateGuess -> pure unit -- TODO
  RegenerateGuess -> pure unit -- TODO
  SolveGame -> pure unit -- TODO

resetState :: forall o. H.HalogenM State Action () o Aff Unit
resetState =
  do state <- H.get
     state' <- case state.currentPage of
                 Game _ -> do gState <- mkGameStateM
                              pure $ defState {useFullDict = state.useFullDict, currentPage = Game $ gState}
                 Solver _ -> pure $ defState {useFullDict = state.useFullDict}
     H.put state'

mkGameStateM :: forall o. H.HalogenM State Action () o Aff GameState
mkGameStateM =
  do state <- H.get
     let wordList = getWordList state
     seed <- H.liftEffect <<< mkGameSeed $ wordList
     pure $ mkGameState seed wordList

pressKeyButton :: forall o. Key -> H.HalogenM State Action () o Aff Unit
pressKeyButton k =
  do
    s <- H.get
    case s.currentPage of
      Game gState -> let gState' = updateBoardAndKeyboard $ gameStateKeyPress gState k
                     in H.put $ s {currentPage = Game gState'}
      _ -> pure unit

pressEnter :: forall o. H.HalogenM State Action () o Aff Unit
pressEnter = do
  s <- H.get
  case s.currentPage of
    Game gState@{sentGuesses, currentGuess}
      | String.length currentGuess /= wordLength -> pure unit
      | not $ currentGuess `elem` (getWordList s) -> alert $ "`" <> currentGuess <> "` is not in the word list."
      | otherwise -> do let wasWin = gameIsWin gState
                        let gState' = gState {sentGuesses = sentGuesses <> [currentGuess], currentGuess = ""}
                        let isWin = gameIsWin gState'
                        let justWon = not wasWin && isWin
                        let gState'' = updateBoardAndKeyboard $ gState' {isWin = wasWin || isWin}
                        H.put $ s {currentPage = Game gState''}
                        if justWon
                        then alert <<< successMessage $ length gState''.sentGuesses
                        else pure unit
    _ -> pure unit -- TODO add functionality for solver page?

getWordList :: State -> Array String
getWordList s
  | s.useFullDict = dictWordList
  | otherwise = wordleWordList

alert :: forall o. String -> H.HalogenM State Action () o Aff Unit
alert s = H.liftEffect $ Web.alert s =<< Web.window

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
      | KE.key event == "Enter" = handleAction $ PressEnter
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

updateBoardAndKeyboard :: GameState -> GameState
updateBoardAndKeyboard gState@{currentWord, sentGuesses, currentGuess} = gState {board = board', keyboard = keyboard'}
  where cells = map (colorRow currentWord) sentGuesses
        board' =
          cells
          <> (if length sentGuesses == maxGuesses then [] else [mkPaddedRow currentGuess])
          <> replicate (maxGuesses - length sentGuesses - 1) (replicate wordLength defCell)
          where mkPaddedRow s = map (\c -> {color: None, letter: c}) (toCharArray s) <> replicate (wordLength - String.length s) defCell
        keyboard' = foldl step empty <<< Array.concat $ cells
        step m {letter, color} = Map.insertWith greener letter color m

colorRow :: String -> String -> Array Cell
colorRow correctWord = gradeGuess (toCharArray correctWord) <<< toCharArray

{- Solver-Page -}

-- TODO
