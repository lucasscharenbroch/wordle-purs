module Main where

import Prelude
import Util
import Page
import State

import Data.Array (range, (!!), length)
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

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall output t. H.Component t Unit output Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just InitKeybinds }
    }
