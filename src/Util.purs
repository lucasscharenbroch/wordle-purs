module Util where

import Prelude

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
