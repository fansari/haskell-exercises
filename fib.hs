#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

import System.Environment

fibStep :: (Int,Int) -> (Int,Int)
fibStep (u,v) = (v,u+v)

fibPair :: Int -> (Int,Int)
fibPair n
  | n == 0     = (0,1)
  | otherwise  = fibStep (fibPair (n-1))

fastFib :: Int -> Int
fastFib = fst . fibPair

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

main = do
  args <- getArgs
  let num = read(head args)::Int
  print(take num fibs)
  -- print(take num [1,2..])


--  print(head x)
--  print(x !! 1)
--  print(fastFib num)
--  print(fibPair num)

{-
example: fibPair 3

fibPair 3 = fibStep (fibPair 2)
fibPair 2 = fibStep (fibPair 1)
fibPair 1 = fibStep (fibPair 0)
fipPair 0 = (0,1)

fibPair 1 = fibStep (fibPair 0) = fibStep (0,1) = (1,1)
fibPair 2 = fibStep (fibPair 1) = fibStep (1,1) = (1,2)
fibPair 3 = fibStep (fibPair 2) = fibStep (1,2) = (2,3)
-}

{-
1 : 1  : 2 : 3 : 5 : 8 : 13 : 21
-}

