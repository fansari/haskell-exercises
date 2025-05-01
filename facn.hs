#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

{---------------------------------------------------------------------------
File    : facn.hs
Date    : 2022-02-26
Author  : Frank Ansari
Purpose : sum of reciprocs of factorial
---------------------------------------------------------------------------}

import System.Environment

fac :: Int -> Int
fac n
  | n < 0   = error "n must not be < 0"
  | n == 0  = 1
  | n == 1  = 1
  | otherwise = n * fac(n-1)

invFac :: Int -> Double
invFac n
  | n < 0   = error "n must be not be < 0"
  | n == 0  = 1
  | n == 1  = 1
  | otherwise = invFac(n-1) / fromIntegral n

sumFac :: Int -> Double
sumFac n = sumFac_r n 1
  where
    sumFac_r :: Int -> Double -> Double
    sumFac_r n s
      | n  == 0 = s
      | otherwise = sumFac_r(n-1) (s + invFac n)

main = do
  args <- getArgs
  let num = read(head args)::Int
  print(sumFac num)
