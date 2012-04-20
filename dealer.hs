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