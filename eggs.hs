#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

import Data.List (find)

-- A rule is a function that takes an Int and returns a Bool
type CountingRule = Int -> Bool

-- Helper to create a remainder rule
mkRemainderRule :: Int -> Int -> CountingRule
mkRemainderRule divisor remainder = \n -> n `mod` divisor == remainder

main :: IO ()
main = do
    let rules = [ mkRemainderRule 2 1
                , mkRemainderRule 3 1
                , mkRemainderRule 4 1
                , mkRemainderRule 5 1
                , mkRemainderRule 6 1
                , mkRemainderRule 7 0
                ]

    -- Use find instead of head to avoid the 'partial function' warning
    -- [1..] is an infinite list
    let result = find (\n -> all (\rule -> rule n) rules) [1..]

    case result of
        Just eggCount -> do
            putStrLn "The woman had at least this many eggs:"
            print eggCount
        Nothing -> putStrLn "No solution found."
