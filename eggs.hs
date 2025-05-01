#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

{---------------------------------------------------------------------------
File    : eggs.hs
Date    : 2022-01-14
Author  : Frank Ansari
Purpose : chinese rest class
---------------------------------------------------------------------------}

type Basket = [(Int,Int)]

eggs :: Basket -> Int
eggs x = (mod (calcSumList x) (prodMod x))

buildModList :: Basket -> [Int]
buildModList (xs) = [ fst x | x <- xs ]

prodMod :: Basket -> Int
prodMod [] = 1
prodMod (x:xs) = fst x * (prodMod xs)

multModList :: [Int] -> Int -> Int
multModList [] n = 1
multModList (x:xs) n =
  case x == n of
    True -> multModList xs n
    False -> x * multModList xs n

calcMod :: Int -> Int ->  Int -> Int
calcMod n m r = calcMod_r n m r 1
  where
    calcMod_r :: Int -> Int -> Int ->  Int -> Int
    calcMod_r n m r k
      | mod (k*n) m  == r = k*n
      | otherwise = calcMod_r n m r (k+1)

calcSumList :: Basket -> Int
calcSumList x = calcSumList_r x
  where
    m = buildModList x
    calcSumList_r :: Basket -> Int
    calcSumList_r [] = 0
    calcSumList_r (x:xs) = calcSumList_r xs + calcMod (multModList m (fst x)) (fst x) (snd x)

checkBasketElement :: (Int,Int) -> Bool
checkBasketElement x
  | fst x < 0 = False
  | snd x < 0 = False
  | fst x > snd x = True
  | otherwise = False

checkBasket :: Basket -> Bool
checkBasket [] = True
checkBasket (x:xs) = checkBasketElement x && checkBasket xs

main = do
  let basket1 = [(3,1),(4,1),(5,1),(7,0)]
  let basket2 = [(3,1),(5,2),(7,3)]

  case checkBasket basket1 of
    True -> putStrLn (show basket1 ++ " => " ++ show (eggs basket1))
    False -> putStrLn ("You have an error in basekt1.")

  case checkBasket basket2 of
    True -> putStrLn (show basket2 ++ " => " ++ show (eggs basket2))
    False -> putStrLn ("You have an error in basekt2.")


--  print (buildModList basket)
--  putStrLn ("mod 3: " ++ (show (multModList (buildModList basket) 3)))
--  putStrLn ("mod 5: " ++ (show (multModList (buildModList basket) 5)))
--  putStrLn ("mod 7: " ++ (show (multModList (buildModList basket) 7)))
--  print(calcMod (multModList (buildModList basket) 3) 3 1)
--  print(multModList (buildModList basket) 5)
--  putStrLn ("mod 3: " ++ (show (calcMod (multModList (buildModList basket) 3) 3 1)))
--  putStrLn ("mod 5: " ++ (show (calcMod (multModList (buildModList basket) 5) 5 2)))
--  putStrLn ("mod 7: " ++ (show (calcMod (multModList (buildModList basket) 7) 7 3)))
--  print (calcSumList basket)
