module Dealer
(
) where

import Cards
import Hands

import Control.Monad.State

dealDealer :: Deck d => d -> (DealerHand, d)
dealDealer d = let
  (down, d') = dealDown d
  (up,  d'') = deal d'
  hand       = DealerHand down up
  in (hand, d'')

dealDealerS :: Deck d => State d DealerHand
dealDealerS = liftS dealDealer

-- Dealer only hits or stands
data DealerAction = Hit | Stand

-- Dealer strategies
type DealerStrategy = Hand -> DealerAction

hitS17 :: DealerStrategy
hitS17 = act . handTotal
  where act (HandValue Bust _)           = Stand
        act (HandValue Soft n) | n <  18 = Hit
                               | n >= 18 = Stand
        act (HandValue Hard n) | n <  17 = Hit
                               | n >= 17 = Stand

standS17 :: DealerStrategy
standS17 = act . handTotal
  where act (HandValue Bust _)           = Stand
        act (HandValue _    n) | n <  18 = Hit
                               | n >= 18 = Stand