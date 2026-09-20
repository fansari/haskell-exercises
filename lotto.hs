#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

import System.Random (newStdGen, randomRs)
import Data.List (sort, sortOn)

main :: IO ()
main = do
    -- Generate values and pair them with an index
    gen <- newStdGen
    let randoms = take 49 (randomRs (0.0, 1.0) gen) :: [Double]

    -- Create the permutation: sort indices by their random values
    -- and then assign ranks 1..49 to these now shuffled indices.
    let ranked = zip (map fst . sortOn snd $ zip [1..49] randoms) [1..49]

    -- Extract ranks of the first six balls and sort the result for the user
    let result = sort [rank | (idx, rank) <- ranked, idx <= 6]

    putStrLn "Die gezogenen Lottozahlen:"
    print result
