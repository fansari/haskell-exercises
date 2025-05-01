#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

{-
File    : collatz.hs
Date    : 2022-01-14
Author  : Frank Ansari
Purpose : calcluate collatz values

max values:
10         [9,19]
100        [97,118]
1000       [871,178]
10000      [6171,261]
100000     [77031,350]
1000000    [837799,524]
10000000   [8400511,685]
-}

import System.Environment

stepCollatz :: Int -> Int
stepCollatz n
  | mod n 2 == 0  = div n 2
  | otherwise = 3*n + 1

buildCollatz :: [Int] -> [Int]
buildCollatz (x:xs)
  | x == 1 = []
  | otherwise = stepCollatz x : buildCollatz [stepCollatz x]

runCollatz :: Int -> [Int]
runCollatz n = n : buildCollatz [n]

lengthCollatz :: Int -> [Int]
lengthCollatz n = [ n, length(buildCollatz [n]) ]

buildLengthTable :: Int -> [[Int]]
buildLengthTable 1 = [[1,0]]
buildLengthTable n = buildLengthTable (n-1) ++ [ lengthCollatz n ]

maxCollatz :: [[Int]] -> [Int]
maxCollatz x = maxCollatz_r x [0,0]
  where
    maxCollatz_r :: [[Int]] -> [Int] -> [Int]
    maxCollatz_r [] n = n
    maxCollatz_r (x:xs) n
      | x!!1 > n!!1 = maxCollatz_r xs x
      | otherwise = maxCollatz_r xs n

main = do
  args <- getArgs
  let num = read(head args)::Int
--  print(runCollatz num)
--  print(maxCollatz [[1,5],[3,9],[8,7],[10,11],[23,2]])
--  print(lengthCollatz 7)
--  print(buildLengthTable 7)
  print(buildLengthTable num)
  print(maxCollatz (buildLengthTable num))
