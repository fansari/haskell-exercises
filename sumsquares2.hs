#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}


-- Eine Liste aller Paare (n, m), für die die Summe der Quadrate ein Quadrat ist
solutions = [ (n, m) | n <- [2..1000]
                     , let s = (n * (n + 1) * (2 * n + 1)) `div` 6
                     , let m = round (sqrt (fromIntegral s))
                     , m * m == s ]

main = print solutions
