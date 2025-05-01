#!/usr/bin/env runghc
{-#  LANGUAGE OverloadedStrings #-}

import Prelude hiding (getLine, Word)

type Word = String
type Line = [Word]

whitespace = ['\n','\t',' ']

lineLen = 20
mylist = ["this","is","some","word","list","for","testing"]

getWord :: String -> String
getWord [] = []
getWord (x:xs)
  | elem x whitespace = []
  | otherwise = x : getWord xs

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
  | elem x whitespace = (x:xs)
  | otherwise = dropWord xs

dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs)
  | elem x whitespace = dropSpace xs
  | otherwise = (x:xs)

splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

split :: String -> [Word]
split [] = []
split st
  = (getWord st) : split (dropSpace (dropWord st))

getLine :: Int -> [Word] -> Line
getLine len [] = []
getLine len (w:ws)
  | length w <= len = w : restOfLine
  | otherwise = []
    where
      newlen = len - (length w + 1)
      restOfLine = getLine newlen ws

dropLine :: Int -> [Word] -> Line
dropLine len [] = []
dropLine len (w:ws)
  | length w <= len =restOfLine
  | otherwise = ws
    where
      newlen = len - (length w + 1)
      restOfLine = dropLine newlen ws

splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines ws
  = getLine  lineLen ws
      : splitLines (dropLine lineLen ws)

main = do
--  print(getLine lineLen mylist)
--  print(dropWord "cat dog")
--  print(dropLine lineLen mylist)
  print(splitLines mylist)
