#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

import System.Environment

primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

twins = [x | x <- zip primes (tail primes), snd x - fst x == 2]

main = do
  args <- getArgs
  let num = read(head args)::Int
  print(take num twins)
