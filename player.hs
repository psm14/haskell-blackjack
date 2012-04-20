module Player
(
) where

import Cards
import Hands

data Player = Player [(Bet, Hand)]
	deriving (Show)

