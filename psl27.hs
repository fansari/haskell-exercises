#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

{--------------------------------------------------------------------------
File    : psl27.hs
Date    : 2023-06-03
Author  : Frank Ansari
Purpose : PSL(2,7)
---------------------------------------------------------------------------}

type Row = (Int, Int)
type Matrix = (Row, Row)

groupZ = 7::Int

{- check whether two matrices are equal -}
isEqualMatrix :: Matrix -> Matrix -> Bool
isEqualMatrix x y
  | (fst $ fst x) == (fst $ fst y) &&
    (snd $ fst x) == (snd $ fst y) &&
    (fst $ snd x) == (fst $ snd y) &&
    (snd $ snd x) == (snd $ snd y) = True
  | otherwise = False

{- check whether a mattrix list contains a matrix -}
isInMatrix :: [Matrix] -> Matrix -> Bool
isInMatrix [] m = False
isInMatrix (x:xs) m
  | isEqualMatrix x m = True
  | otherwise = isInMatrix xs m

{- calculate the determinante of a matrix m in Zq -}
calcDet :: Matrix -> Int -> Int
calcDet m q = mod ((fst $ fst m) * (snd $ snd m)
              - (fst $ snd m) * (snd $ fst m)) q

{- create list of all pairs of intergers from 0 to n-1
e.g. for n=7: [(0,0), (0,1), ..., (6,6)] -}
createRows :: Int -> [Row]
createRows max = createRows_h max 0 0
  where
    createRows_h :: Int-> Int-> Int -> [Row]
    createRows_h max m n
      | m == max = []
      | otherwise = (createRows_h max (m+1) n) ++ (createRows_r max m n)
      where
        createRows_r :: Int -> Int -> Int -> [Row]
        createRows_r max m n
          | n == max = []
          | otherwise = [(n,m)] ++ (createRows_r max m (n+1))

{- create a list of all possible Rows from a row list -}
createMatrix :: [Row] -> [Matrix]
createMatrix [] = []
createMatrix r = createMatrix_h r r
  where
    createMatrix_h :: [Row] -> [Row] -> [Matrix]
    createMatrix_h r  [] = []
    createMatrix_h r (x:xs)
      | xs == [] = createMatrix_r x r
      | otherwise = createMatrix_r x r ++ createMatrix_h r xs
      where
        createMatrix_r :: Row -> [Row] -> [Matrix]
        createMatrix_r p [] = []
        createMatrix_r p (x:xs)
          | xs == [] = [(p,x)]
          | otherwise = [(p,x)] ++ createMatrix_r p xs

{- make matrix list unique -}
uniqueMatrix :: [Matrix] -> [Matrix]
uniqueMatrix [] = []
uniqueMatrix m = uniqueMatrix_r m []
  where
    uniqueMatrix_r :: [Matrix] -> [Matrix] -> [Matrix]
    uniqueMatrix_r [] m = m
    uniqueMatrix_r (x:xs) m
     | xs == [] = if (isInMatrix m x) then m else [x] ++ m
     | otherwise = if (isInMatrix m x)
                   then uniqueMatrix_r xs m
                   else uniqueMatrix_r xs ([x] ++ m)

{- filter all elements of a matrix lis
where the determinante of the matrix is 1 -}
filterMatrix :: [Matrix] -> [Matrix]
filterMatrix [] = []
filterMatrix m = filterMatrix_r m []
  where
    filterMatrix_r :: [Matrix] -> [Matrix] -> [Matrix]
    filterMatrix_r [] m = m
    filterMatrix_r (x:xs) m
      | xs == [] = if (calcDet x groupZ) == 1 then [x] ++ m else m
      | otherwise = if (calcDet x groupZ) == 1
                    then filterMatrix_r xs ([x] ++ m)
                    else filterMatrix_r xs m

{- format matrix to string -}
formatMatrixElement :: Matrix -> String
formatMatrixElement m = "((" ++ (show (fst $ fst m)) ++ ","
                        ++ (show (snd $ fst m)) ++ ")"
                        ++ ", (" ++ (show (fst $ snd m))
                        ++ "," ++ (show (snd $ snd m)) ++ "))"

{- format matrix list to string
w: elements per line, m: :matrix list -}
formatMatrix :: Int -> [Matrix] -> String
formatMatrix w [] = ""
formatMatrix w m = "[\n" ++ formatMatrix_r 0 w m ++ "\n]"
  where
    formatMatrix_r :: Int -> Int ->[Matrix] -> String
    formatMatrix_r n w (x:xs)
      | xs == [] = formatMatrixElement x
      | otherwise = if (mod (n+1) w) == 0
                    then formatMatrixElement x ++ ",\n"
                           ++ formatMatrix_r (n+1) w xs
                    else formatMatrixElement x ++ ", "
                           ++ formatMatrix_r (n+1) w xs

main = do
  putStrLn $ formatMatrix 8 $ filterMatrix $ createMatrix $ createRows groupZ
  putStrLn $  "\nThe quotient group {I, -I} has "
              ++ (show $ div (length $ filterMatrix
              $ createMatrix $ createRows groupZ) 2)
              ++ " elements."
