module State where

import Data.Tuple
import Prelude
import Util
import Wordle

import Control.Monad.Rec.Class (forever)
import Data.Array (elem, foldl, length, range, replicate, uncons, zipWith, (!!), (\\), unsnoc, take)
import Data.Array as Array
import Data.Map (Map, empty)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe)
import Data.String as String
import Data.String.CodeUnits (charAt, fromCharArray, singleton, toCharArray)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Util (backspaceText, whenElem)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document, alert) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import WordList (dictWordList, wordleWordList)

{- Types -}

data Page = Game GameState
          | Solver SolverState

data TestStatus = Testing H.SubscriptionId TestState
                  | DoneTesting TestState
                  | NotTesting

type TestState =
  { wordsToGo :: Array String
  , one :: Int
  , two :: Int
  , three :: Int
  , four :: Int
  , five :: Int
  , six :: Int
  , failed :: Int
  }

mkDefTestState :: Array String -> TestState
mkDefTestState wordsToGo =
  { wordsToGo
  , one: 0
  , two: 0
  , three: 0
  , four: 0
  , five: 0
  , six: 0
  , failed: 0
  }

type GameState =
  { keyboard :: KeyboardState
  , currentWord :: String
  , sentGuesses :: Array String
  , currentGuess :: String
  , board :: Board
  , testStatus :: TestStatus
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
  , board: defBoard
  , testStatus: NotTesting
  }

type SolverState =
  { guesses :: Array String
  , board :: Board
  , sentColorings :: Array (Array Color)
  , currentColoring :: Array Color
  , invalidWords :: Array String
  }

defSolverState :: SolverState
defSolverState =
  { guesses: []
  , board: defBoard
  , sentColorings: []
  , currentColoring: []
  , invalidWords: []
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
            | TestStep
            | StopTesting

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
  GenerateGuess -> generateGuess
  RegenerateGuess -> regenerateGuess
  PressColorKey c -> pressColorButton c
  SolveGame -> solveGame
  TestAllWords -> testAllWords
  TestStep -> testStep
  StopTesting -> stopTesting

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
                        let gState' = updateBoardAndKeyboard $ gState {sentGuesses = sentGuesses <> [currentGuess], currentGuess = ""}
                        let isWin = gameIsWin gState'
                        let justWon = not wasWin && isWin
                        H.put $ s {currentPage = Game gState'}
                        if justWon
                        then alert <<< successMessage $ length gState'.sentGuesses
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
gameStateKeyPress state@{sentGuesses, currentGuess} (KLetter c)
  | gameIsWin state = state
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

solveGame :: forall o. H.HalogenM State Action () o Aff Unit
solveGame =
  do
    state <- H.get
    case state.currentPage of
      Solver _ -> pure unit
      Game gState -> do gState' <- solveGameState (getWordList state) gState
                        H.put $ state {currentPage = Game $ gState'}

solveGameState :: forall o. Array String -> GameState -> H.HalogenM State Action () o Aff GameState
solveGameState wordList gState
  | gameIsWin gState || length gState.sentGuesses == maxGuesses = pure gState
  | otherwise = case pickGuess validWords of
                  Nothing -> alert "Ran out of possible words." *> pure gState
                  Just newGuess -> solveGameState wordList gState'
                    where gState' = updateBoardAndKeyboard $ gState {sentGuesses = gState.sentGuesses <> [newGuess], currentGuess = ""}
    where validWords = wordsFittingBoard wordList gState.board

{- Solver-Page -}

generateGuess :: forall o. H.HalogenM State Action () o Aff Unit
generateGuess =
  do state <- H.get
     case state.currentPage of
       Game _ -> pure unit
       Solver sState@{guesses, sentColorings, currentColoring, board, invalidWords}
        | length guesses == maxGuesses -> pure unit
        | length guesses /= 0 && length currentColoring < wordLength -> pure unit
        | otherwise -> let possibleGuesses = wordsFittingBoard (getWordList state) board \\ invalidWords
                       in case pickGuess possibleGuesses of
                            Nothing -> alert "Out of valid words."
                            Just s -> H.put $ state {currentPage = Solver sState'}
                              where
                                Tuple sentColorings' currentColoring' = case length guesses of
                                  0 -> Tuple sentColorings currentColoring
                                  _ -> Tuple (sentColorings <> [currentColoring]) []
                                sState' = updateSolverBoard $
                                               sState { guesses = sState.guesses <> [s]
                                                 , sentColorings = sentColorings'
                                                 , currentColoring = currentColoring'
                                                 }

regenerateGuess :: forall o. H.HalogenM State Action () o Aff Unit
regenerateGuess =
  do state <- H.get
     case state.currentPage of
       Game _ -> pure unit
       Solver sState@{guesses, invalidWords, sentColorings} ->
         do H.put $ state {currentPage = Solver sState'}
            generateGuess
         where
           Tuple guesses' invalidWords' = case unsnoc guesses of
             Just {init, last} -> Tuple init (invalidWords <> [last])
             Nothing -> Tuple [] invalidWords
           Tuple sentColorings' currentColoring' = case unsnoc sentColorings of
             Just {init, last} -> Tuple init last
             Nothing -> Tuple [] []
           sState' = sState { guesses = guesses'
                            , invalidWords = invalidWords'
                            , sentColorings = sentColorings'
                            , currentColoring = currentColoring'
                            }

pressColorButton :: forall o. Color -> H.HalogenM State Action () o Aff Unit
pressColorButton color =
  do state <- H.get
     case state.currentPage of
      Solver sState@{currentColoring, guesses} -> H.put state {currentPage = Solver sState'}
        where sState' = updateSolverBoard $ sState {currentColoring = coloring'}
              coloring'
                | length guesses == 0 = currentColoring
                | color == None = case unsnoc currentColoring of
                  Just {init, last} -> init
                  Nothing -> currentColoring
                | otherwise = take 5 $ currentColoring <> [color]
      _ -> pure unit

updateSolverBoard :: SolverState -> SolverState
updateSolverBoard sState@{guesses, sentColorings, currentColoring} = sState {board = board'}
  where colorings = sentColorings <> [currentColoring <> replicate 5 None]
        board' =
          zipWith toCells guesses colorings
          <> replicate (maxGuesses - length guesses) (replicate 5 {letter: ' ', color: None})
        toCells s colors = zipWith (\letter color -> {letter, color}) (toCharArray s) colors

{- Testing -}

testAllWords :: forall o. H.HalogenM State Action () o Aff Unit
testAllWords =
  do state <- H.get
     case state.currentPage of
      Solver _ -> pure unit
      Game gState -> do
                        subId <- H.subscribe =<< timer TestStep
                        H.put $ state {currentPage = Game $ gState {testStatus = Testing subId <<< mkDefTestState $ getWordList state}}

testStep :: forall o. H.HalogenM State Action () o Aff Unit
testStep =
  do state <- H.get
     case state.currentPage of
       Game gState@{testStatus: Testing subId tState} ->
         do let tState'
                  | gameIsWin gState = case length gState.sentGuesses of
                     1 -> tState {one = tState.one + 1}
                     2 -> tState {two = tState.two + 1}
                     3 -> tState {three = tState.three + 1}
                     4 -> tState {four = tState.four + 1}
                     5 -> tState {five = tState.five + 1}
                     6 -> tState {six = tState.six + 1}
                     _ -> tState -- shouldn't happen
                  | otherwise = tState {failed = tState.failed + 1}
            case Array.uncons tState.wordsToGo of
              Nothing -> do H.put $ state {currentPage = Game $ gState {testStatus = DoneTesting tState'}}
                            H.unsubscribe subId
              Just {head, tail} -> do H.put $ state {currentPage = Game $ gState'}
                                      handleAction SolveGame
                where gState' = (mkGameState 0 [head]) {testStatus = Testing subId tState''}
                      tState'' = tState' {wordsToGo = tail}
       _ -> pure unit -- STOP the cycle

stopTesting :: forall o. H.HalogenM State Action () o Aff Unit
stopTesting =
  do state <- H.get
     case state.currentPage of
          Game gState@{testStatus: Testing subId tState} ->
            do H.put $ state {currentPage = Game $ gState {testStatus = DoneTesting tState}}
               H.unsubscribe subId
          _ -> pure unit


-- from https://github.com/purescript-halogen/purescript-halogen/blob/master/docs/guide/04-Lifecycles-Subscriptions.md
timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Aff.Milliseconds 10.0
    H.liftEffect $ HS.notify listener val
  pure emitter
