#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

module PSL25 where

import Data.List (nub)

-- Matrix is defined as a 4-tuple of Integers (a, b, c, d) representing:
-- [ a  b ]
-- [ c  d ]
type Matrix = (Int, Int, Int, Int)

-- Matrix multiplication in the finite field F5 (Modulo 5)
mul :: Matrix -> Matrix -> Matrix
mul (a1, b1, c1, d1) (a2, b2, c2, d2) =
    ( (a1*a2 + b1*c2) `mod` 5
    , (a1*b2 + b1*d2) `mod` 5
    , (c1*a2 + d1*c2) `mod` 5
    , (c1*b2 + d1*d2) `mod` 5
    )

-- Calculate the determinant (ad - bc) modulo 5
det :: Matrix -> Int
det (a, b, c, d) = (a*d - b*c) `mod` 5

-- In the Projective Special Linear group PSL(2,5), the matrices M and -M
-- are considered the same element. To avoid duplicates, we normalize
-- by picking a representative where the first non-zero entry is "positive" (1 or 2).
normalize :: Matrix -> Matrix
normalize (a, b, c, d)
    | isNegative  = ((-a) `mod` 5, (-b) `mod` 5, (-c) `mod` 5, (-d) `mod` 5)
    | otherwise   = (a, b, c, d)
    where
      -- Logic: Values 3 and 4 are treated as -2 and -1 in F5.
      isNegative = a > 2 || (a == 0 && b > 2) || (a == 0 && b == 0 && c > 2)

-- Generate all 60 elements of the PSL(2,5) group
-- 1. Take all combinations of a,b,c,d from {0..4}
-- 2. Filter for matrices with determinant 1
-- 3. Normalize to account for the Projective property (P)
-- 4. Use 'nub' to remove duplicates after normalization
psl25 :: [Matrix]
psl25 = nub [ normalize (a, b, c, d)
            | a <- [0..4], b <- [0..4], c <- [0..4], d <- [0..4]
            , det (a, b, c, d) == 1 ]

main :: IO ()
main = do
    let elements = psl25
    putStrLn $ "Total number of elements: " ++ show (length elements)
    -- Print each matrix in the list
    mapM_ print elements
