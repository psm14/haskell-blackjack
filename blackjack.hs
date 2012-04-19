module Blackjack where

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State

import Cards
import Hands

data Game = Game Hand [Hand]

instance Show Game where
  show (Game h hs) = "Game " ++ (show h) ++ " " ++ (show hs)

deal' :: Int -> State Deck [Card]
deal' n = get >>= \d -> put (remain d) >> return (dealt d)
  where dealt  d = take n d
        remain d = drop n d

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

-- TEST CODE, player strategies will eventually be at least (Game -> Action) with the count factoring in somewhere
playStep :: (Hand -> Action) -> Hand -> State Deck [Hand]
playStep strat (Hand Done h) = return [Hand Done h]
playStep strat h = play (strat h) h

playHand :: (Hand -> Action) -> Hand -> State Deck [Hand]
playHand strat h = let
  step = playStep strat
  in if (handDone h == True) then
    return $ [h]
  else do
    res <- step h
    sheet (playHand strat) res

allPlayersDone :: [Hand] -> Bool
allPlayersDone = all handDone 

allPlayersDone' :: Game -> Bool
allPlayersDone' (Game _ hs) = allPlayersDone hs

sheet :: (Hand -> State Deck [Hand]) -> [Hand] -> State Deck [Hand]
sheet f hs = let
  mapped  = fmap f hs
  reduced = sequence mapped
  in liftM concat reduced

playPlayers :: (Hand -> Action) -> Game -> State Deck Game
playPlayers strat (Game d hs) = if (allPlayersDone hs == False) then do 
                                  hs' <- sheet (playHand strat) hs
                                  playPlayers strat (Game d hs')
                                else 
                                  return $ Game d hs

playGame :: (Hand -> Action) -> Game -> State Deck Game
playGame strat g = (playPlayers strat g) >>= (playDealer strat)

dealAndPlay :: Int -> (Hand -> Action) -> State Deck Game
dealAndPlay p strat = (newGame' p) >>= (playGame strat)

allTotals :: Game -> (HandValue, [HandValue])
allTotals (Game d hs) = let
  dtot = handTotal' d
  ptot = fmap handTotal' hs
  in (dtot, ptot)
