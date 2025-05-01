#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

f :: Integer -> Integer -> Integer
f a b
  | b==0 = 1
  | odd b = mod (a * f a(b-1)) m
  | otherwise = f (mod (a^2) m) (div b 2)
  where m = 10^200

main = do
  print(iterate (f 3) 3 !! 200)
