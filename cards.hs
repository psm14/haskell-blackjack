module Cards 
( stdDeck
, multiDeck
, shuffle
, shuffleIO
, aceInTheHole
, tenInTheHole
, Card(..)
, DownCard(..)
, Suit(..)
, Rank(..)
, StdDeck(..)
, Deck(..)
) where

import System.Random.Shuffle (shuffle')
import System.Random (RandomGen, newStdGen, split)
import Control.Monad (liftM)
import Control.Monad.State

data Suit = Clubs | Hearts | Spades | Diamonds
     deriving (Show, Read, Eq, Ord, Enum)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
     deriving (Show, Read, Eq, Ord, Enum)

data Card = Card Rank Suit
     deriving (Show, Read, Eq, Ord)

data DownCard = DownCard Card

data StdDeck = Deck [Card]

instance Show StdDeck where
  show d = "Deck"

-- Typeclass to enable adding counts to a deck
-- deal: Deal card off top of deck and update count if needed
-- dealDown: Deal card "face down" (card isn't counted)
-- turn: "Turn over" the down card and thus update the count on the passed in deck
-- remain: Cards remaining in deck, needed for true count

-- The following have default implementations:
-- deal': Deal n cards off deck and update card
-- liftS: Lift a function operating on a deck into the State monad
-- dealS, dealS': Lifted deal and deal', respectively
class Deck d where
  deal     :: Deck d => d -> (Card, d)
  dealDown :: Deck d => d -> (DownCard, d)
  turn     :: Deck d => DownCard -> d -> (Card, d)
  remain   :: Deck d => d -> Int

  -- Optional
  deal'    :: Deck d => Int -> d -> ([Card], d)
  deal' 1 d = let
    (c, d') = deal d
    in ([c], d')
  deal' n d = let
    (c, d') = deal d
    (cs, e) = deal' (n - 1) d'
    in (c:cs, e)

  -- Derived
  liftS    :: Deck d => (d -> (a, d)) -> State d a
  liftS  = state

  dealS    :: Deck d => State d Card
  dealS  = liftS deal

  dealS'   :: Deck d => Int -> State d [Card]
  dealS' = liftS . deal'

  -- Why not
  lowerS   :: Deck d => State d a -> d -> (a, d)
  lowerS = runState

-- Plain old deck (no count)
instance Deck StdDeck where
  deal (Deck d) = (card, rest)
     where card = head d
           rest = Deck $ tail d

  deal' n (Deck d) = (cards, rest)
       where cards = take n d
             rest  = Deck $ drop n d

  dealDown (Deck d) = (card, rest)
     where card = DownCard $ head d
           rest = Deck     $ tail d

  turn (DownCard c) d = (c, d)

  remain (Deck d) = length d

stdDeck :: StdDeck
stdDeck = Deck [Card r s | r <- [Two .. Ace], s <- [Clubs .. Diamonds]]

multiDeck :: Int -> StdDeck -> StdDeck
multiDeck n (Deck d) = Deck . concat . replicate n $ d

shuffle :: RandomGen g => StdDeck -> g -> StdDeck
shuffle (Deck d) g = Deck $ shuffle' d (length d) g

shuffleIO :: StdDeck -> IO StdDeck
shuffleIO d = liftM (shuffle d) newStdGen

-- Legal ways to peek at the downcard
-- Make sure to 'turn' it still
aceInTheHole :: DownCard -> Bool
aceInTheHole (DownCard c)  = isAce c
  where isAce (Card Ace _) = True
        isAce _            = False

tenInTheHole :: DownCard -> Bool
tenInTheHole (DownCard c) = isTen c
  where isTen (Card r _)  | r >= Ten  = True
                          | otherwise = False
