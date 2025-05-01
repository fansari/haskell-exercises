#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

{--------------------------------------------------------------------------
File    : a5.hs
Date    : 2023-05-13
Author  : Frank Ansari
Purpose : alternate group A5
--------------------------------------------------------------------------}

type PosPair = (Int, Int)
type PosPairs = (PosPair, PosPair)

getA5String = "ABCDE"::String

{-the position of the char in the original string represents its number
get the number from the char -}
translateNum :: Int -> Char
translateNum n
  | n < 0 = 'x'
  | n >= length getA5String = 'x'
  | otherwise =getA5String!!n

{- the position of the char in the original string represents its number
get the char from the number -}
translateChar :: Char -> Int
translateChar c = findPos getA5String $ c

{- find the position of the char in the string -}
findPos :: String -> Char -> Int
findPos s c = findPos_r s c 0
  where
    findPos_r :: String -> Char -> Int -> Int
    findPos_r s c n
      | n >= length s = -1
      | c == s!!n = n
      | otherwise = findPos_r s c (n+1)

{- put the char on the position of int in the string -}
putElement :: String -> Char -> Int -> String
putElement s c n
  |  n < 0 = ""
  |  n >= length s = ""
  | otherwise = putElement_r s c 0 n
    where
      putElement_r :: String -> Char -> Int -> Int -> String
      putElement_r s c m n
        | m >= length s = s
        | m == n = (take n s) ++ c:[] ++ drop (m+1) s
        | otherwise = putElement_r s c (m+1) n

{- the translation of the pospair integers respresent the chars
e.g. for "abcde" (0,2) means swap 'a' with ''c'
for "cdabe" (0,2) gives "adcbe" -}
swapElements :: String -> PosPair -> String
swapElements "" p = ""
swapElements s p = putElement (putElement s c n) d m
  where
   c = translateNum $ fst p
   d = translateNum $ snd p
   m = findPos s c
   n = findPos s d

{- swap chars of string according to PosPair
e.g. "abcde" ((0,1), (1,2)) -> "cabde" -}
swapDouble :: String -> PosPairs -> String
swapDouble s p = swapElements (swapElements s $ fst p) $ snd p

{- do a swapDouble on a list of PosPairs -}
swapDoubles :: String -> [PosPairs] -> [String]
swapDoubles s [] = []
swapDoubles s (x:xs)
  | xs == [] = [swapDouble s x]
  | otherwise = [swapDouble s x] ++ swapDoubles s xs

{- create a list of all possible PosPairs from a PosPair list -}
createDoublePairs :: [PosPair] -> [PosPairs]
createDoublePairs [] = []
createDoublePairs (x:xs)
  | xs == [] = []
  | otherwise = createDoublePairs_h ([x] ++ xs) ++ createDoublePairs xs
  where
    createDoublePairs_h :: [PosPair] -> [PosPairs]
    createDoublePairs_h [] = []
    createDoublePairs_h (x:xs)
      | xs == [] = []
      | otherwise = createDoublePairs_r x xs
      where
        createDoublePairs_r :: PosPair -> [PosPair] -> [PosPairs]
        createDoublePairs_r p [] = []
        createDoublePairs_r p (x:xs)
          | xs == [] = [(p,x)]
          | otherwise = [(p,x)] ++ createDoublePairs_r p xs

{- remove all PosPairs of a list which occur
more than once (make list unique) -}
filterPairs :: [PosPair] -> [PosPair]
filterPairs [] = []
filterPairs x = filterPairs_r x []
  where
    filterPairs_r :: [PosPair] -> [PosPair] -> [PosPair]
    filterPairs_r (x:xs) r
      | xs == [] = appendPairIf r x
      | otherwise  = appendPairIf (filterPairs_r xs r) x

{- append a PosPair to a PosPair list
if the PosPair is not already in the list -}
appendPairIf :: [PosPair] -> PosPair -> [PosPair]
appendPairIf [] p = [p]
appendPairIf (x:xs) p
  | comparePairs x p == True = (x:xs)
  | xs == [] = if (comparePairs x p) == False then [x] ++ [p] else []
  | otherwise = [x] ++ appendPairIf xs p

{- compare two PosPairs -}
comparePairs :: PosPair -> PosPair -> Bool
comparePairs x y
 | (fst x == fst y) && (snd x == snd y) = True
 | (fst x == snd y) && (snd x == fst y) = True
 | otherwise = False

{- create list of all pairs of intergers from 0 to n-1
e.g. for n=5: [(0,1), (0,2), ..., (3,4)] -}
createPairs :: Int -> [PosPair]
createPairs max
  | max == 0 = []
  | otherwise = (createPairs (max-1)) ++ (createPairs_r 0 (max-1))
  where
    createPairs_r :: Int -> Int -> [PosPair]
    createPairs_r n max
      | n == max = []
      | otherwise = [(n,max)] ++ (createPairs_r (n+1) max)

{- format a list of strings with newlines
put w elements in one line -}
formatStringList :: Int -> [String] -> String
formatStringList n [] = ""
formatStringList w s = formatStringList_r 0 w s
  where
    formatStringList_r :: Int -> Int -> [String] -> String
    formatStringList_r n w [] = ""
    formatStringList_r n w (x:xs)
     | (mod (n+1) w) == 0 = x ++ "\n" ++ formatStringList_r (n+1) w xs
     | otherwise = x ++ " " ++ formatStringList_r (n+1) w xs

{- append a string to a string list
if the string is not already in the list -}
appendStringIf :: [String] -> String -> [String]
appendStringIf [] s = [s]
appendStringIf (x:xs) s
  | x == s = (x:xs)
  | xs == [] = if x == s then [x] else[x] ++ [s]
  | otherwise = [x] ++ appendStringIf xs s

{- make string list unique -}
filterStringList :: [String] -> [String]
filterStringList [] = []
filterStringList s = filterStringList_r [] s
  where
    filterStringList_r :: [String] -> [String] -> [String]
    filterStringList_r s (x:xs)
     | xs == [] = appendStringIf s x
     | otherwise = appendStringIf (filterStringList_r s xs) x

{- create all strings you can find with one swapDoubles step -}
createStringList :: String -> [String]
createStringList "" = []
createStringList s = swapDoubles s $ createDoublePairs $ createPairs $ length s

{- create all strings -}
createFullStringList :: [String]
createFullStringList = createFullStringList_r $ createStringList getA5String
  where
    createFullStringList_r :: [String] -> [String]
    createFullStringList_r [] = []
    createFullStringList_r (x:xs) = createStringList x ++ createFullStringList_r xs

{- create group A5 -}
createGroupA5 :: [String]
createGroupA5 = filterStringList createFullStringList

main = do
  putStrLn $ formatStringList 16 createGroupA5
  print $ length createGroupA5
