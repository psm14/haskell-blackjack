module HiLoCount 
( HiLoDeck
, hiLoDeck
, runningCount
, trueCount
) where

import Data.List

import Cards

type HiLoCount = Int

hiLoValue :: Card -> HiLoCount
hiLoValue (Card r _) | r <= Six  = 1
                     | r <= Nine = 0
                     | otherwise = -1
  

hiLoSum :: HiLoCount -> Card -> HiLoCount
hiLoSum a b = a + (hiLoValue b)

-- Strict fold prevents busting the stack when summing a large number of cards
hiLoTotal' :: [Card] -> HiLoCount
hiLoTotal' cs = foldl' hiLoSum 0 cs 

hiLoTotal :: [Card] -> HiLoCount
hiLoTotal = sum . map hiLoValue

-- Wrap a standard deck and track the Hi-Lo count as cards are dealed off of it
data (Deck d) => HiLoDeck d = HiLoDeck d HiLoCount

instance (Deck d, Show d) => Show (HiLoDeck d) where
  show (HiLoDeck d c) = show d ++ " (RC: " ++ show c ++ ", TC: " ++ (show $ trueCount (HiLoDeck d c)) ++ ")"

instance (Deck d) => Deck (HiLoDeck d) where
  deal (HiLoDeck d n) = let
    (c, d') = deal d
    n'      = hiLoSum n c
    in (c, HiLoDeck d' n')

  deal' n (HiLoDeck d m) = let
    (cs, d') = deal' n d
    m'       = m + hiLoTotal cs
    in (cs, HiLoDeck d' m')

  dealDown (HiLoDeck d n) = let
    (c, d') = dealDown d
    in (c, HiLoDeck d' n)

  turn c (HiLoDeck d n) = let
    (c', d') = turn c d
    n'       = hiLoSum n c'
    in (c', HiLoDeck d' n')

  remain (HiLoDeck d _) = remain d

hiLoDeck :: Deck d => d -> HiLoDeck d
hiLoDeck d = HiLoDeck d 0

runningCount :: Deck d => HiLoDeck d -> Int
runningCount (HiLoDeck _ c) = c

trueCount :: Deck d => HiLoDeck d -> Double
trueCount (HiLoDeck d c) = (fromIntegral c) / decksRemain
       where decksRemain = (fromIntegral $ remain d) / 52