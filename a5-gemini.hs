#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

import Data.List (permutations)

isEvenPerm :: Ord a => [a] -> Bool
isEvenPerm xs = even . length $ [ (i,j) | i <- [0..n-1], j <- [i+1..n-1], xs !! i > xs !! j ]
  where n = length xs

createGroupA5 :: [String]
createGroupA5 = filter isEvenPerm (permutations "ABCDE")

main :: IO ()
main = do
    mapM_ putStrLn [unwords chunk | chunk <- chunksOf 16 createGroupA5]
    print (length createGroupA5)
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
