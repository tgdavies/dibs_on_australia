module Main where

import Prelude

import DOM

import Data.Maybe
import Data.Array
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Int
import Data.Foldable

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random
import Control.Monad.Eff.DOM
import Control.Monad.Eff.AUI
import Control.Bind

data ChanceCard eff = ChanceCard { probability :: Number, text :: Eff eff String }

data ChanceDeck eff = ChanceDeck { title :: String, cards :: Array (ChanceCard eff) }

totalProbability :: forall eff. Array (ChanceCard eff) -> Number
totalProbability cards =
    foldl (\sum (ChanceCard c) -> sum + c.probability) (0.0 :: Number) cards

s :: forall eff. String -> Eff eff String
s str = do
    return str

card :: forall eff. Number -> Eff eff String -> ChanceCard eff
card p t = ChanceCard {probability: p, text: t}

randomI n = do
    r <- random
    return (floor (r * (toNumber n)))

blownOffCourse = do
    direction <- randomI 6
    distance <- randomI 5
    return ("Blown off course, go " ++ (show $ distance + 1) ++ " hexes in direction " ++ (show $ direction + 1))

allsWell p = ChanceCard {probability: p, text: s "All's well."}


--decks ::  forall eff. Array (ChanceDeck eff)
decks = [
    ChanceDeck {
        title: "Sea",
        cards: [
            allsWell 80.0,
            card 5.0 (s "Rats eat one point of stores"),
            card 5.0 (s "blackmail mutiny - 1 point of stores"),
            card 5.0 (s "hit a rock miss a turn")
        ]
    },
    ChanceDeck {
        title: "Storms",
        cards: [
            allsWell 40.0,
            card 10.0 (s "Mast Damaged, miss a turn"),
            card 10.0 (s "One point of stores lost overboard."),
            card 30.0 blownOffCourse,
            card 5.0 (s "hit a rock miss a turn")
        ]
    },
    ChanceDeck {
        title: "Island",
        cards: [
            card 50.0 (s "Replenished supplies."),
            card 25.0 (s "Ran aground, miss a turn.")
        ]
    },
    ChanceDeck {
        title: "Reef",
        cards: [
            allsWell 50.0,
            card 25.0 (s "Hit a reef, miss a turn"),
            card 25.0 (s "Hit a reef, one point of stores lost.")
        ]
    },
    ChanceDeck {
        title: "Pirates",
        cards: [
            allsWell 50.0,
            card 10.0 (s "Pirates plunder one point of stores."),
            card 10.0 (s "Pirates plunder two points of stores."),
            card 10.0 (s "Pirates board your ship, miss a turn."),
            card 10.0 (s "Cannon fire damages hull, miss a turn.")
        ]
        }
    ]

button :: forall eff. String -> Eff (dom :: DOM | eff) Node
button title = do
    node <- createElement "button"
    addClass "aui-button" node
    setInnerHTML title node
    return node

--render :: forall eff. Node -> ChanceDeck eff -> Eff (dom :: DOM, random :: RANDOM | eff) Unit
--render :: forall t171. Node -> { title :: String, cards :: Array { text :: Eff (random :: RANDOM, dom :: DOM | t171) String, probability :: Number }} -> Eff (random :: RANDOM, dom :: DOM | t171) Unit
render node deck =
    let
        title (ChanceDeck d) = d.title
    in do
        buttonNode <- button (title deck)
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

probabilityToIndex :: forall eff. Array (ChanceCard eff) -> Number -> Int
probabilityToIndex as p =
    (foldl (\state (ChanceCard a) -> if state.p - a.probability < 0.0 then {p: -1.0, i:state.i} else {p: state.p - a.probability, i: state.i+1}) {p: p, i:0} as).i

--chooseCard :: forall eff. ChanceDeck eff -> Number -> Eff (random :: RANDOM | eff) String
chooseCard (ChanceDeck deck) n =
    let
        total = totalProbability deck.cards
        i = probabilityToIndex deck.cards (total * n)
        getText (ChanceCard c) = c.text
    in
        getText (indexWithDefault deck.cards i (card 0.0 (s "error")))

--showCard :: forall eff. ChanceDeck eff -> Node -> Eff (dom :: DOM, random :: RANDOM | eff) Unit
showCard deck node = do
    test <- createElement "span"
    i <- random
    text <- chooseCard deck i
    dialog { width: 400, height: 200, id: "card-dialog", closeOnOutsideClick: true, contents: text }
    return unit

--renderDecks :: forall eff. Eff (dom :: DOM, random :: RANDOM | eff) Unit
renderDecks = do
  Just chanceContainer <- querySelector "#chanceContainer"
  foldM (\u deck -> render chanceContainer deck) unit decks
  return unit

main :: Eff (console :: CONSOLE, dom :: DOM, random :: RANDOM) Unit
main = do
  renderDecks
