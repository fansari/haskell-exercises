#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

import System.Environment

primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

twins = [x | x <- zip primes (tail primes), snd x - fst x == 2]

calcBrun :: [(Int,Int)] -> Double
calcBrun p = calcSum p 0
  where
    calcPair :: (Int,Int) -> Double
    calcPair (x,y) = 1/(fromIntegral x) + 1/(fromIntegral y)
    calcSum :: [(Int,Int)] -> Double -> Double
    calcSum [] s = s
    calcSum (x:xs) s = (calcPair x) + (calcSum xs s)

main = do
  args <- getArgs
  let num = read(head args)::Int
  print(calcBrun (take num twins))
