#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

import System.Random (newStdGen, randomRs)
import Data.List (sortOn)

-- | Main entry point for the lottery simulation
main :: IO ()
main = do
    -- 1. Generate 49 random numbers between 0.0 and 1.0
    gen <- newStdGen
    let randomFloats = take 49 (randomRs (0.0, 1.0) gen) :: [Double]
    print randomFloats
    putStrLn ""

    -- 2. Pair indices (1..49) with the random values
    let indexedValues = zip [1..49] randomFloats
    print indexedValues
    putStrLn ""

    -- 3. Sort by the random value to determine the rank
    let sortedByValue = sortOn snd indexedValues
    print sortedByValue
    putStrLn ""

    -- 4. Assign ranks 1 to 49 based on sorted position
    let rankedNumbers = zip (map fst sortedByValue) [1..49]
    print rankedNumbers

    -- 5. Take the ranks assigned to the first six indices
    let lottoNumbers = [rank | (idx, rank) <- rankedNumbers, idx <= 6]

    putStrLn "Die gezogenen Lottozahlen sind:"
    print lottoNumbers
