module Wordle where

import Prelude

import Data.Array
import Data.Tuple
import Data.Function (on)
import Data.String.CodeUnits (toCharArray)

data Color = Green | Yellow | Gray | None

greener :: Color -> Color -> Color
greener Green _ = Green
greener _ Green = Green
greener Yellow _ = Yellow
greener _ Yellow = Yellow
greener _ _ = Gray

type Cell =
  { color :: Color
  , letter :: Char
  }

defCell :: Cell
defCell = {color: None, letter: ' '}


type Board = Array (Array Cell) -- 6 x 5

defBoard :: Board
defBoard = replicate 6 (replicate 5 defCell)


wordLength :: Int
wordLength = 5

maxGuesses :: Int
maxGuesses = 6

gradeGuess_ :: String -> String -> Array Cell
gradeGuess_ = on gradeGuess toCharArray

gradeGuess :: Array Char -> Array Char -> Array Cell
gradeGuess correct given = zipWith (\letter color -> {letter, color}) given $ colors
    where
          step (Tuple _ unmatched) (Tuple letter isGreen)
            | isGreen = (Tuple Green unmatched)
            | letter `elem` unmatched = (Tuple Yellow (delete letter unmatched))
            | otherwise = (Tuple Gray unmatched)
          colors = map fst <<< scanl step (Tuple Gray unmatched) $ zip given isGreen
            where
              unmatched = map fst <<< filter (not <<< snd) $ zip correct isGreen
              isGreen = zipWith (==) given correct
