module Main where

import Prelude

import DOM


import Data.Maybe
import Data.Array
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Int

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random
import Control.Bind

import Control.Monad.Eff.DOM
import Control.Monad.Eff.AUI

type ChanceDeck = { title :: String, cards :: Array String }

decks :: Array ChanceDeck
decks = [
    { title: "Sea", cards: ["All's well", "All's well", "Rats eat one point of stores"]},
    { title: "Storms", cards: ["All's well", "Mast Damaged, miss a turn", "One point of stores lost", "Blown off course"]},
    { title: "Island", cards: ["Replenshed supplies", "Ran aground, miss a turn"]}
    ]

button :: String -> forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Node
button title = do
    node <- createElement "button"

    return node

render :: Node -> ChanceDeck -> forall eff. Eff (console :: CONSOLE, dom :: DOM, random :: RANDOM | eff) Unit
render node deck = do
    buttonSpan <- createElement "button"
    addClass "aui-button" buttonSpan
    setInnerHTML deck.title buttonSpan
    deckDiv <- createElement "p"
    appendChild buttonSpan deckDiv
    appendChild deckDiv node
    addEventListener "click" (showCard deck deckDiv) buttonSpan
    return unit

chooseCard :: ChanceDeck -> Number -> Maybe String
chooseCard deck n =
    let
      i = floor (n * (toNumber $ length deck.cards))
    in
      index deck.cards i

showCard :: ChanceDeck -> Node -> forall eff. Eff (console :: CONSOLE, dom :: DOM, random :: RANDOM | eff) Unit
showCard deck node = do
    test <- createElement "span"
    i <- random
    -- setInnerHTML (fromMaybe "Error" (chooseCard deck i)) test
    -- appendChild test node
    dialog { width: 400, height: 200, id: "card-dialog", closeOnOutsideClick: true, contents: fromMaybe "Error" (chooseCard deck i) }
    return unit

setupEventHandlers :: forall eff. Eff (console :: CONSOLE, dom :: DOM, random :: RANDOM | eff) Unit
setupEventHandlers = do
  Just chanceContainer <- querySelector "#chanceContainer"
  foldM (\u deck -> render chanceContainer deck) unit decks
  -- render {title: "Test", cards: ["A", "B", "C", "D"]} chanceContainer
  -- render {title: "Test2", cards: ["A", "B", "C", "D"]} chanceContainer

  return unit

main :: forall eff. Eff (console :: CONSOLE, dom :: DOM, random :: RANDOM | eff) Unit
main = do
  log "Attaching event handlers"
  setupEventHandlers
