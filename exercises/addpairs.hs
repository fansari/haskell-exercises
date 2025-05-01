#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ m+n | (m,n) <- pairList, m<n ]

main = do
  let list = [(2,3),(2,1),(7,8)]
  print(list)
  let list = [(8,9)]
  print(list)
  print(addPairs list)
