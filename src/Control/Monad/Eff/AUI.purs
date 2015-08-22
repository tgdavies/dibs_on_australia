module Control.Monad.Eff.AUI where

import Prelude

import DOM

import Data.Maybe
import Data.Foreign
import Data.Function

import Control.Monad.Eff

type Dialog = {width :: Int, height :: Int, id :: String, closeOnOutsideClick :: Boolean, contents :: String}

foreign import dialog :: forall eff. Dialog -> Eff (dom :: DOM | eff) Unit
