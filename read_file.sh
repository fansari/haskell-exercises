#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

main = do
  x <- readFile "test.txt"
  putStr x
