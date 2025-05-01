#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

import System.Random
import Data.List

rolls :: RandomGen g => Int -> g -> [Word]
rolls n = take n . unfoldr (Just . uniformR (1, 6))

main = do
  g <- newStdGen
  let x = rolls 100 g :: [Word]
  print x
