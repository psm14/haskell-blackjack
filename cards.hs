module Cards 
( stdDeck
, multiDeck
, shuffle
, shuffleIO
, infiniteDeck
, infiniteShuffler
, Card(..)
, Suit(..)
, Rank(..)
, Deck
) where

import System.Random.Shuffle (shuffle')
import System.Random (RandomGen, newStdGen, split)
import Control.Monad (liftM)

data Suit = Clubs | Hearts | Spades | Diamonds
     deriving (Show, Read, Eq, Ord, Enum)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
     deriving (Show, Read, Eq, Ord, Enum)

data Card = Card Rank Suit
     deriving (Show, Read, Eq, Ord)

data Deck = Deck [Card]

instance Show Deck where
  show d = "Deck"

stdDeck :: Deck
stdDeck = Deck [Card r s | r <- [Ace .. King], s <- [Clubs .. Diamonds]]

multiDeck :: Int -> Deck -> Deck
multiDeck n (Deck d) = Deck . concat . replicate n $ d

shuffle :: RandomGen g => Deck -> g -> Deck
shuffle (Deck d) g = Deck $ shuffle' d (length d) g

shuffleIO :: Deck -> IO Deck
shuffleIO d = liftM (shuffle d) newStdGen

infiniteDeck :: Deck -> Deck
infiniteDeck (Deck d) = Deck $ concat decks
   where decks = d : decks

infiniteShuffler :: RandomGen g => Deck -> g -> Deck
infiniteShuffler (Deck d) g = Deck . concat $ decks g
       where decks g = d : decks g'
             g'      = snd $ split g

infiniteShufflerIO :: Deck -> IO Deck
infiniteShufflerIO d = liftM (infiniteShuffler d) newStdGen

-- TODO? Make Deck into a typeclass so that other implementations can include a count
deal :: Deck -> (Card, Deck)
deal (Deck d) = (card, rest)
   where card = head d
         rest = Deck $ tail d