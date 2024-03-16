module Wordle where

import Data.Array
import Data.Foldable as Foldable
import Data.Maybe
import Data.Tuple
import Prelude
import Util

import Data.Function (on)
import Data.Map (Map)
import Data.Map as Map
import Data.String.CodeUnits (toCharArray)

data Color = Green | Yellow | Gray | None

derive instance colorEq :: Eq Color

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

wordsFittingBoard :: Array String -> Board -> Array String
wordsFittingBoard words board = filter fitsConstraints words
  where
    fitsConstraints s = fitsPosConstraints board s && fitsCntConstraints board s

-- yellows => net minimum char count
fitsCntConstraints :: Board -> String -> Boolean
fitsCntConstraints board s = Map.intersection minCnts guessCnts == minCnts
                          && Foldable.all id (Map.intersectionWith (<=) minCnts guessCnts)
  where
    minCnts = foldl (Map.unionWith max) Map.empty <<< map rowMinCnts $ board
    rowMinCnts = countIntoMap <<< map (\c -> c.letter) <<< filter (\c -> c.color == Yellow)
    guessCnts = countIntoMap <<< toCharArray $ s

-- greens => exact position match
-- yellows => position mismatch
fitsPosConstraints :: Board -> String -> Boolean
fitsPosConstraints board s = all id $ zipWith ($) charFns (toCharArray s)
  where
    charFns = foldl (zipWith (\f g x -> f x && g x)) (replicate 5 $ const true) cellFns
    cellFns = map (map getCellConstraint) board
    getCellConstraint {letter, color} =
      case color of
        Green -> \c -> c == letter
        Yellow -> \c -> c /= letter
        _ -> const true

-- pickGuess :: Array String -> Maybe String
-- pickGuess = ?what
