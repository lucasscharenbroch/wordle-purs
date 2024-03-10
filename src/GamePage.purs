module GamePage where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Util (whenElem)

type State =
  { showInfo :: Boolean
  , showSettings :: Boolean
  }

data Action = HideInfoBox

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

infoBox :: forall w. HH.HTML w Action
infoBox =
  HH.div
    [HP.id "messageBox"]
    [ HH.h1_ [HH.text "Wordle"]
    , HH.text "Wordle is a fun word-guessing game. You have six attempts to guess a secret word."
    , HH.br_
    , HH.br_
    , HH.text "Enter words with the letter-buttons or keyboard, and press enter to guess."
    , HH.br_
    , HH.br_
    , HH.text "The box will be green if the letter and position is correct. The box will be yellow if the letter is in the word, but not in that position. The box will be gray if the letter does not exist in the word (beyond the boxes already colored)."
    , HH.br_
    , HH.br_
    , HH.text "Press \"solve\", and the solver will automatically make guesses for you."
    , HH.br_
    , HH.button
        [ HP.classes [ HH.ClassName "okButton" ]
        , HE.onClick \_ -> HideInfoBox
        ]
        [ HH.text "OK" ]
    ]

render :: forall w. State -> HH.HTML w Action
render state =
    HH.main_
      [ whenElem state.showInfo (const infoBox)
      ]

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  HideInfoBox -> H.modify_ (\s -> s {showInfo = false})

initialState :: Unit -> State
initialState _ =
  { showInfo: true
  , showSettings: false
  }

component :: forall output m t. H.Component t Unit output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
