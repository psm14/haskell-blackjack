module HiLoCount where

import Data.List

import Cards

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

hiLoSum :: HiLoCount -> Card -> HiLoCount
hiLoSum a b = a + (hiLoValue b)

-- Strict fold prevents busting the stack when summing a large number of cards
hiLoTotal' :: [Card] -> HiLoCount
hiLoTotal' cs = foldl' hiLoSum 0 cs 

hiLoTotal :: [Card] -> HiLoCount
hiLoTotal = sum . map hiLoValue
