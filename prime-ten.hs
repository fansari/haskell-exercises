#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

import System.Environment

main = do
  args <- getArgs
  let num = read(head args)::Integer
  print(mod (10^num - 10) (num^2))
  return()
