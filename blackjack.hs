import System.Random.Shuffle (shuffle')
import System.Random (RandomGen, newStdGen, split)
import Data.Monoid
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State

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

type Deck = [Card]

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

infiniteDeck :: RandomGen gen => Deck -> gen -> Deck
infiniteDeck deck gen = concat $ repeatingDecks deck gen
  where repeatingDecks deck gen = (shuffle deck gen) : (repeatingDecks deck (snd $ split gen))

randInfShuffle :: Deck -> IO Deck
randInfShuffle deck = newStdGen >>= doShuffle deck
  where doShuffle deck gen = return $ infiniteDeck deck gen

data HandStatus = Play
                | Done
     deriving (Show)

data Hand = Hand HandStatus [Card]
     deriving (Show)

data HandType = Hard
              | Soft
              | Bust
     deriving (Show, Read, Eq)

data HandValue = HandValue HandType Int
     deriving (Show, Read, Eq)

-- Note that this only is well-defined for hand values that are valid in BlackJack (i.e Using a Soft 2 or a Bust 10 will have strange behavior)
instance Monoid HandValue where
  mempty  = HandValue Hard 0
  mappend (HandValue Soft m) (HandValue t n)    = if (m+n <= 21) then HandValue Soft (m+n) else mappend (HandValue Hard (m-10)) (HandValue t n) 
  mappend (HandValue t m) (HandValue Soft n)    = mappend (HandValue Soft n) (HandValue t m)
  mappend (HandValue Hard m) (HandValue Hard n) = if (m+n <= 21) then HandValue Hard (m+n) else HandValue Bust (m+n)
  mappend (HandValue t m) (HandValue u n)       = HandValue Bust (m+n)

cardTotal :: Card -> HandValue
cardTotal n = case n of
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


handTotal :: [Card] -> HandValue
handTotal = mconcat . (map cardTotal)

handTotal' :: Hand -> HandValue
handTotal' (Hand _ cs) = handTotal cs

hand' :: [Card] -> Hand
hand' cs = Hand (checkDone $ handTotal cs) cs
  where checkDone (HandValue Bust _) = Done
        checkDone _                  = Play

data Game = Game Hand [Hand]

instance Show Game where
  show (Game h hs) = "Game " ++ (show h) ++ " " ++ (show hs)

deal' :: Int -> State Deck [Card]
deal' n = get >>= \d -> (put $ drop n d) >> (return $ take n d)

deal :: State Deck Card
deal = liftM head $ deal' 1

newGame' :: Int -> State Deck Game
newGame' n = do
  roundOne <- deal' (n+1)
  roundTwo <- deal' (n+1)
  dealer   <- return $ hand' [(head roundOne), (head roundTwo)]
  players  <- return $ zipWith (\a b -> hand' [a,b]) (tail roundOne) (tail roundTwo)
  return $ Game dealer players

newGame :: State Deck Game
newGame = newGame' 1

doHit :: Hand -> State Deck [Hand]
doHit (Hand Play hs) = deal >>= (\c -> return $ [hand' (hs ++ [c])])
doHit _ = undefined

doStand :: Hand -> State Deck [Hand]
doStand (Hand Play h) = return $ [Hand Done h]
doStand _ = undefined

doDouble :: Hand -> State Deck [Hand]
doDouble (Hand Play hs) = deal >>= (\c -> return $ [Hand Done (hs ++ [c])])

doSplit :: Hand -> State Deck [Hand]
doSplit (Hand Play [one, two]) = do
  [one', two'] <- deal' 2
  return $ [hand' [one, one'], hand' [two, two']]
doSplit _ = undefined

-- Identical to stand for now, might change somehow once bets are tracked
doSurrender :: Hand -> State Deck [Hand]
doSurrender = doStand


play :: Action -> Hand -> State Deck [Hand]
play a h = case a of
  Hit    -> doHit h
  Stand  -> doStand h
  Double -> doDouble h
  Split  -> doSplit h

-- Dealer play (Assume only hit or stand)
play' :: Action -> Hand -> State Deck Hand
play' a h = (play a h) >>= \hs -> return $ head hs

data Action = Hit
            | Stand
            | Double
            | Split
            | Surrender

-- Hit Soft 17
dealerStrategy :: Hand -> Action
dealerStrategy h = case (handTotal' h) of
  HandValue Bust _  -> Stand
  HandValue Soft 18 -> Stand
  HandValue Soft 19 -> Stand
  HandValue Soft 20 -> Stand
  HandValue Soft 21 -> Stand
  HandValue Soft _  -> Hit
  HandValue Hard 17 -> Stand
  HandValue Hard 18 -> Stand
  HandValue Hard 19 -> Stand
  HandValue Hard 20 -> Stand
  HandValue Hard 21 -> Stand
  HandValue Hard _  -> Hit

playDealer :: (Hand -> Action) -> Game -> State Deck Game
playDealer _ (Game (Hand Done h) hs) = return $ Game (Hand Done h) hs
playDealer strat (Game hand hs) = do
  hand' <- play' (strat hand) hand
  playDealer strat (Game hand' hs)

type HiLoCount = Int

hiLoValue :: Card -> HiLoCount
hiLoValue n = case n of
  Card Two _   -> 1
  Card Three _ -> 1
  Card Four _  -> 1
  Card Five _  -> 1
  Card Six _   -> 1
  Card Seven _ -> 0
  Card Eight _ -> 0
  Card Nine _  -> 0
  Card Ten _   -> -1
  Card Jack _  -> -1
  Card Queen _ -> -1
  Card King _  -> -1
  Card Ace _   -> -1

hiLoTotal :: [Card] -> HiLoCount
hiLoTotal = reduce' . map'
  where map'    = map hiLoValue
        reduce' = sum