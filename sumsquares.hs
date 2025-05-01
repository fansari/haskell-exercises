#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

{---------------------------------------------------------------------------
File    : sumsquares.hs
Date    : 2022-07-30
Author  : Frank Ansari
Purpose : find n and m so that
          1² + 2² + 3² + ... + n² = m²
          with n, m ∈ ℕ and n > 1
---------------------------------------------------------------------------}

isInt :: Double -> Bool
isInt x = x == fromIntegral (round x)

sumSquares :: Int -> Int
sumSquares x = (x * (x + 1) * (2*x + 1)) `div` 6

checkSqrt :: Int -> Bool
checkSqrt x = isInt(sqrt (fromIntegral (sumSquares x)))

findSquares :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
findSquares x max r
  | x > max = r
  | otherwise = findSquares (x+1) max (buildSquarelist x r)
  where
    buildSquarelist :: Int -> [(Int,Int)] -> [(Int,Int)]
    buildSquarelist x r
      | checkSqrt x = r ++ [(x, round (sqrt (fromIntegral (sumSquares x))))]
      | otherwise = r

main = do
  print(findSquares 2 1000 [])
