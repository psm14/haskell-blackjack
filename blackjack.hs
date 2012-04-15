import System.Random.Shuffle (shuffle')
import System.Random (RandomGen, newStdGen)
import Data.Monoid

data Suit = Clubs
          | Hearts
          | Spades
          | Diamonds
     deriving (Show, Read, Eq, Ord, Enum)

data Rank = Ace
          | Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
     deriving (Show, Read, Eq, Ord, Enum)

data Card = Card Rank Suit
     deriving (Show, Read, Eq, Ord)

type HoleCard = Card
type Deck = [Card]
type Hand = [Card]

data DealerHand = DealerHand Card Hand
     deriving (Show, Read, Eq)

data HandStatus = Hard
                | Soft
                | Bust
     deriving (Show, Read, Eq)

data HandValue = HandValue HandStatus Int
     deriving (Show, Read, Eq)

stdDeck :: Deck
stdDeck = [Card r s | r <- [Ace .. King], s <- [Clubs .. Diamonds]]

multiDeck :: Int -> Deck
multiDeck 0 = []
multiDeck n = stdDeck ++ multiDeck (n-1)

shuffle :: RandomGen gen => Deck -> gen -> Deck
shuffle deck gen = shuffle' deck (length deck) gen

randShuffle :: Deck -> IO Deck
randShuffle deck = newStdGen >>= doShuffle deck
  where doShuffle deck gen = return $ shuffle deck gen

cardTotal :: Card -> HandValue
cardTotal n = case n of
                Card Ace _   -> HandValue Soft 11
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

instance Monoid HandValue where
  mempty  = HandValue Hard 0
  mappend (HandValue Soft m) (HandValue t n)    = if (m+n <= 21) then HandValue Soft (m+n) else HandValue t (m+n - 10) 
  mappend (HandValue t m) (HandValue Soft n)    = if (m+n <= 21) then HandValue Soft (m+n) else HandValue t (m+n - 10)
  mappend (HandValue Hard m) (HandValue Hard n) = if (m+n <= 21) then HandValue Hard (m+n) else HandValue Bust (m+n)
  mappend (HandValue Bust m) (HandValue Hard n) = HandValue Bust (m+n)
  mappend (HandValue Hard m) (HandValue Bust n) = HandValue Bust (m+n)
