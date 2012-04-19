module Hands
( Hand(..)
, HandStatus(..)
, HandType(..)
, HandValue(..)
, cardValue
) where

import Data.Monoid

import Cards

data Hand = Hand HandStatus [Card]
     deriving (Show)

data HandStatus = Play
                | Done
     deriving (Show)

data HandType = Hard
              | Soft
              | Bust
     deriving (Show, Eq)

data HandValue = HandValue HandType Int
     deriving (Show, Eq)

-- Note that this only is well-defined for hand values that are valid in BlackJack (i.e Using a Soft 2 or a Bust 10 will have strange behavior)
instance Monoid HandValue where
  mempty  = HandValue Hard 0
  mappend (HandValue Soft m) (HandValue t n)    = if (m+n <= 21) then HandValue Soft (m+n) else mappend (HandValue Hard (m-10)) (HandValue t n) 
  mappend (HandValue t m) (HandValue Soft n)    = mappend (HandValue Soft n) (HandValue t m)
  mappend (HandValue Hard m) (HandValue Hard n) = if (m+n <= 21) then HandValue Hard (m+n) else HandValue Bust (m+n)
  mappend (HandValue t m) (HandValue u n)       = HandValue Bust (m+n)

cardValue :: Card -> HandValue
cardValue n = case n of
  Card Two _   -> HandValue Hard 2
  Card Three _ -> HandValue Hard 3
  Card Four _  -> HandValue Hard 4
  Card Five _  -> HandValue Hard 5
  Card Six _   -> HandValue Hard 6
  Card Seven _ -> HandValue Hard 7
  Card Eight _ -> HandValue Hard 8
  Card Nine _  -> HandValue Hard 9
  Card Ten _   -> HandValue Hard 10
  Card Jack _  -> HandValue Hard 10
  Card Queen _ -> HandValue Hard 10
  Card King _  -> HandValue Hard 10
  Card Ace _   -> HandValue Soft 11

handTotal :: Hand -> HandValue
handTotal = handTotal' . cards
  where cards (Hand _ cs) = cs 

handTotal' :: [Card] -> HandValue
handTotal' = mconcat . fmap cardValue

cardStatus :: [Card] -> HandStatus
cardStatus cs = done $ handTotal' cs
  where done (HandValue Bust _) = Done
        done _                  = Play

hand :: [Card] -> Hand
hand cs = Hand (cardStatus cs) cs
  
handDone :: Hand -> Bool
handDone (Hand Done _) = True
handDone _             = False