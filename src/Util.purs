module Util where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Array (foldl)
import Data.Array as Array
import Data.CodePoint.Unicode (isLetter) as Unicode
import Data.Maybe (fromMaybe)
import Data.String (codePointFromChar)
import Data.String as String
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

whenElem :: forall w i. Boolean -> (Unit -> HH.HTML w i) -> HH.HTML w i
whenElem cond f = if cond then f unit else HH.text ""

backspaceText :: forall w i. HH.HTML w i
backspaceText =
    HH.i
      [HP.classes
          [ HH.ClassName "fa"
          , HH.ClassName "fa-arrow-left"
          ]]
      []

toUpper :: Char -> Char
toUpper c =  fromMaybe '_' <<< Array.head <<< toCharArray <<< String.toUpper <<< fromCharArray $ [c]

isLetter :: Char -> Boolean
isLetter = Unicode.isLetter <<< codePointFromChar

blackbird :: forall a b c d. (c -> d) -> (a -> b -> c) -> a -> b -> d
blackbird = (<<<) <<< (<<<)

infixr 8 blackbird as .:

id :: forall a. a -> a
id x = x

countIntoMap :: forall a. Ord a => Array a -> Map a Int
countIntoMap = foldl (\m k -> Map.insertWith (+) k 1 m) Map.empty
