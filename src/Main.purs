module Main where

import Prelude

import Data.List

import Halogen
import Halogen.Util
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import qualified Control.Monad.Eff.Console as Console

import Data.Maybe

type Choice = { title :: String }
type App = { choices :: List Choice, title :: String }

data Query a = Add a |
               Remove a |
               UpdateDescription String a

renderApp :: forall g. (Functor g) => Component App Query g
renderApp = component render eval where
  render :: App -> ComponentHTML Query
  render app = H.div_ [
    H.div_
      [ H.h1_ [ H.text "Poople" ]
      , H.input [ E.onValueChange (E.input UpdateDescription) ]
      , H.button
          [ E.onClick (E.input_ Add) ]
          [ H.text "Add choice" ]
      , H.button
          [ E.onClick (E.input_ Remove) ]
          [ H.text "Remove choice" ]
      ]
    , H.div_ $ fromList $ map renderChoiceView app.choices
    ]

  renderChoiceView choice = H.h1_ [ H.text $ "Choice: " ++ choice.title]

  eval :: Natural Query (ComponentDSL App Query g)
  eval (Add next) = do
    modify addChoice
    pure next
  eval (Remove next) = do
    modify removeChoice
    pure next
  eval (UpdateDescription desc next) = do
    modify (updateDescription desc)
    pure next

updateDescription :: String -> App -> App
updateDescription title app = app { choices = app.choices, title = title }

addChoice :: App -> App
addChoice app = app { choices = { title : app.title } : app.choices }

removeChoice :: App -> App
removeChoice app = app { choices = fromMaybe Nil $ tail app.choices }

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI renderApp { choices : Nil, title: "" }
  appendToBody app.node
