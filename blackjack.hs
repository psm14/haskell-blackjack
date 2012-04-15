import System.Random.Shuffle (shuffle')
import System.Random (RandomGen, newStdGen)
import Control.Monad.State (State, state)

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

data HandValue = Hard Int
               | Soft Int
               | Bust Int
     deriving (Show, Read, Eq)

data Play = Stand
          | Hit
          | Double
          | Split
          | Surrender
     deriving (Show, Read)

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

--dealPlayer :: Deck -> (Deck, Hand)
--dealPlayer d = (remaining d, hand d)
--  where remaining = drop 2
--        hand      = take 2

--dealDealer :: Deck -> (Deck, DealerHand)
--dealDealer d = (remaining d, dealerHand $ hand d)
--  where remaining         = drop 2
--        hand              = take 2
--        dealerHand (a:bs) = DealerHand a bs

cardTotal :: Card -> HandValue
cardTotal n = case n of
                Card Ace _   -> Soft 11
                Card Two _   -> Hard 2
                Card Three _ -> Hard 3
                Card Four _  -> Hard 4
                Card Five _  -> Hard 5
                Card Six _   -> Hard 6
                Card Seven _ -> Hard 7
                Card Eight _ -> Hard 8
                Card Nine _  -> Hard 9
                Card Ten _   -> Hard 10
                Card Jack _  -> Hard 10
                Card Queen _ -> Hard 10
                Card King _  -> Hard 10

-- TODO: There has to be a way to make this cleaner
addCards :: HandValue -> HandValue -> HandValue
addCards (Soft n) (Soft m) = if (n+m <= 21) then Soft (n+m) else Soft (n+m-10)
addCards (Hard n) (Soft m) = if (n+m <= 21) then Soft (n+m) else Hard (n+m-10)
addCards (Soft n) (Hard m) = if (n+m <= 21) then Soft (n+m) else Hard (n+m-10)
addCards (Hard n) (Hard m) = if (n+m <= 21) then Hard (n+m) else Bust (n+m)
addCards (Bust n) (Hard m) = Bust (n+m)
addCards (Hard n) (Bust m) = Bust (n+m)
addCards (Soft n) (Bust m) = Bust (n+m-10)
addCards (Bust n) (Soft m) = Bust (n+m-10)

handTotal :: Hand -> HandValue
handTotal = foldr (addCards . cardTotal) (Hard 0)

revealDealer :: DealerHand -> Hand
revealDealer (DealerHand hole hand) = hole : hand

--playHand :: Deck -> Hand -> Play -> (Deck, Hand)
--playHand d h p = case p of
--                   Hit    -> (drop 1 d, h : take 1 d)
--                   Stand  -> (d, h)
--                   Double -> (drop 1 d, h : take 1 d)


loadShoe :: Deck -> State Deck ()
loadShoe deck = state $ \s -> ((),deck ++ s) 

deal :: State Deck Card
deal = state $ \(c:cs) -> (c,cs)

dealDealer :: State Deck DealerHand
dealDealer = state $ \(cs) -> (dealerHand $ hand cs, remaining cs)
  where remaining         = drop 2
        hand              = take 2
        dealerHand (a:bs) = DealerHand a bs

dealPlayer :: State Deck Hand
dealPlayer = state $ \(cs) -> (hand cs, remaining cs)
  where remaining = drop 2
        hand      = take 2
