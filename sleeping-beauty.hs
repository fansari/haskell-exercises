#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

{-
  coin: 0 => heads
        1 => tails

  week: [Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday]
-}

import System.Random
import Data.List
import Debug.Trace

-- normal coin with probability 1/2 for each side
coin :: RandomGen g => Int -> g -> [Word]
coin n = take n . unfoldr (Just . uniformR (0, 1))

-- altered coin with probability of 1/10 for tails
coin2 :: RandomGen g => Int -> g -> [Word]
coin2 n = map (\c -> if c == (0 :: Word)  then 1 else 0)
          . take n . unfoldr (Just . uniformR (0, 9))

stepWeek :: (Int,Int) -> Word -> (Int,Int)
stepWeek z c
  | c == 0 = (foldl isAwake (fst z) (week 0), snd z)
  | c == 1 = (fst z, foldl isAwake (snd z) (week 1))
  where
    week:: Int -> [Int]
    week c
      | c == 1 = [0,1,1,0,0,0,0]
      | otherwise = [0,1,0,0,0,0,0]
    isAwake:: Int -> Int -> Int
    isAwake e w
      | w == 1 && c == 0 = traceShow(e + 1, snd z) e + 1
      | w == 1 && c == 1 = traceShow(fst z, e + 1) e + 1
      | otherwise = e

runDice :: [Word] -> (Int,Int) -> (Int,Int)
runDice [] z = z
runDice c z = foldl stepWeek z c

main = do
  g <- newStdGen
  let c = coin 1000 g
  let x = runDice c (0,0)
  print c
  print x
