module Wordle where

import Data.Array
import Data.Maybe
import Data.Tuple
import Prelude
import Util

import Data.Foldable as Foldable
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
-- grays => net maximum char count
fitsCntConstraints :: Board -> String -> Boolean
fitsCntConstraints board s = Map.intersection minCnts guessCnts == minCnts
                          && Foldable.all id (Map.intersectionWith (<=) minCnts guessCnts)
                          && Foldable.all id (Map.intersectionWith (>=) maxCnts guessCnts')
  where
    minCnts = foldl (Map.unionWith max) Map.empty <<< map rowMinCnts $ board
    maxCnts = foldl Map.union Map.empty $ map rowMaxCnts $ board
    rowToColorCountMap color = countIntoMap <<< map (\c -> c.letter) <<< filter (\c -> c.color == color)
    rowMinCnts = rowToColorCountMap Yellow
    rowMaxCnts row = mapWithKey (\c _ -> lookupOr 0 c yellows + lookupOr 0 c greens) grays
      where grays = rowToColorCountMap Gray $ row
            yellows = rowToColorCountMap Yellow $ row
            greens = rowToColorCountMap Green $ row
    guessCnts = countIntoMap <<< toCharArray $ s
    guessCnts' = foldl (\m c -> Map.insertWith max c 0 m) guessCnts <<< toCharArray $ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


data CurriedEqualityCheck a = CurriedEq a
                            | CurriedNe a

derive instance curriedEqalityCheckEq :: Eq a => Eq (CurriedEqualityCheck a)

applyCurriedEqualityCheck :: forall a. Eq a => CurriedEqualityCheck a -> a -> Boolean
applyCurriedEqualityCheck (CurriedEq x) y = x == y
applyCurriedEqualityCheck (CurriedNe x) y = x /= y

-- greens => exact position match
-- yellows, grays => position mismatch
fitsPosConstraints :: Board -> String -> Boolean
fitsPosConstraints board s = all id $ zipWith ($) charFns (toCharArray s)
  where
    charFns = map conjChecks charChecks
    conjChecks checks x = all (flip ($) x) checks'
      where checks' = map applyCurriedEqualityCheck $ nubEq checks
    charChecks :: Array (Array (CurriedEqualityCheck Char))
    charChecks = foldl (zipWith (<>)) (replicate 5 []) cellChecks
    cellChecks :: Array (Array (Array (CurriedEqualityCheck Char)))
    cellChecks = map (map getCellConstraint) board
    getCellConstraint :: Cell -> Array (CurriedEqualityCheck Char)
    getCellConstraint {letter, color} =
      case color of
        Green -> [CurriedEq letter]
        Yellow -> [CurriedNe letter]
        Gray -> [CurriedNe letter]
        None -> []

pickGuess :: Array String -> Maybe String
pickGuess = head -- TODO
