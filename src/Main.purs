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
import Control.Monad.Eff.DOM
import Control.Monad.Eff.AUI
import Control.Bind



type ChanceDeck = { title :: String, cards :: Array (forall eff. Eff (random :: RANDOM | eff) String) }

s :: String -> forall eff. Eff (random :: RANDOM | eff) String
s str = do
    return str

s2 :: String -> forall eff. Eff (random :: RANDOM | eff) String
s2 str = do
    n <- random
    return $ show n

randomI n = do
    r <- random
    return (floor (r * (toNumber n)))


blownOffCourse = do
    direction <- randomI 6
    distance <- randomI 5
    return ("Blown off course, go " ++ (show $ distance + 1) ++ " hexes in direction " ++ (show $ direction + 1))

allsWell = s "All's well."


decks :: Array ChanceDeck
decks = [
    { title: "Sea", cards: [allsWell, allsWell, s "Rats eat one point of stores"]},
    { title: "Storms", cards: [allsWell, s "Mast Damaged, miss a turn", s "One point of stores lost overboard.", blownOffCourse]},
    { title: "Island", cards: [s "Replenished supplies.", s "Ran aground, miss a turn."]}
    ]

button :: String -> forall eff. Eff (dom :: DOM | eff) Node
button title = do
    node <- createElement "button"
    addClass "aui-button" node
    setInnerHTML title node
    return node

render :: Node -> ChanceDeck -> forall eff. Eff (dom :: DOM, random :: RANDOM | eff) Unit
render node deck = do
    buttonNode <- button deck.title
    deckDiv <- createElement "p"
    appendChild buttonNode deckDiv
    appendChild deckDiv node
    addEventListener "click" (showCard deck deckDiv) buttonNode
    return unit

indexWithDefault :: forall a. Array a -> Int -> a -> a
indexWithDefault as i default =
    case index as i of
        Nothing -> default
        Just a -> a

chooseCard :: ChanceDeck -> Number -> forall eff. Eff (random :: RANDOM | eff) String
chooseCard deck n =
    let
        i = floor (n * (toNumber $ length deck.cards))
    in do
        indexWithDefault deck.cards i (s "error")

--chooseCard2 :: ChanceDeck -> Number -> forall eff. Eff (random :: RANDOM | eff) String
--chooseCard2 deck n =
--    let
--        i = floor (n * (toNumber $ length deck.cards))
--    in
--        case index deck.cards i of
--            Nothing -> s "error"
--            Just f -> f

showCard :: ChanceDeck -> Node -> forall eff. Eff (dom :: DOM, random :: RANDOM | eff) Unit
showCard deck node = do
    test <- createElement "span"
    i <- random
    text <- chooseCard deck i
    dialog { width: 400, height: 200, id: "card-dialog", closeOnOutsideClick: true, contents: text }
    return unit

renderDecks :: forall eff. Eff (dom :: DOM, random :: RANDOM | eff) Unit
renderDecks = do
  Just chanceContainer <- querySelector "#chanceContainer"
  foldM (\u deck -> render chanceContainer deck) unit decks
  return unit

main :: Eff (console :: CONSOLE, dom :: DOM, random :: RANDOM) Unit
main = do
  renderDecks
