module Main where

import qualified Data.Set as Set
import System.Random (randomRIO)
import Control.Monad (replicateM)

type Permutation = [Int]

identity :: Permutation
identity = [0..10]

-- Multiply two permutations
multiply :: Permutation -> Permutation -> Permutation
multiply p1 p2 = map (\i -> p1 !! (p2 !! i)) [0..10]

-- Explore the group for a given genB
exploreGroup :: Permutation -> Maybe [Permutation]
exploreGroup genB = go (Set.singleton identity) [identity]
  where
    genA = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0]
    go visited [] = if Set.size visited == 7920 then Just (Set.toList visited) else Nothing
    go visited currentLayer
      | Set.size visited > 7920 = Nothing -- Optimization: Early abort
      | otherwise =
          let nexts = [multiply p genA | p <- currentLayer] ++ [multiply p genB | p <- currentLayer]
              newElements = Set.fromList nexts `Set.difference` visited
          in if Set.null newElements
             then if Set.size visited == 7920 then Just (Set.toList visited) else Nothing
             else go (Set.union visited newElements) (Set.toList newElements)

-- Fixed randomInvolution to correctly handle the monadic actions
randomInvolution :: IO Permutation
randomInvolution = do
    -- We need to generate the pair of random numbers first
    pairs <- replicateM 5 $ do
        a <- randomRIO (0, 10)
        b <- randomRIO (0, 10)
        return (a, b)
    -- Now we fold over the list of pairs (Int, Int)
    return $ foldl (\acc (a, b) -> swap a b acc) identity pairs
  where
    swap i j l = map (\x -> if x == i then l!!j else if x == j then l!!i else x) l

main :: IO ()
main = search 0
  where
    search attempt = do
      -- Output status update every 1000 attempts
      if attempt `mod` 1000 == 0
        then putStrLn $ "Still searching... current attempt: " ++ show attempt
        else return ()

      genB <- randomInvolution
      case exploreGroup genB of
        Just _  -> formatResult attempt genB
        Nothing -> search (attempt + 1)

-- Output the result in the exact Haskell-specific format: genB = [x, y, z, ...]
formatResult :: Int -> Permutation -> IO ()
formatResult attempt p = do
    putStrLn $ "Success! Found a valid genB at attempt " ++ show attempt ++ ":"
    putStrLn $ "genB = [" ++ formatList p ++ "]"
    -- Objective verification of fixed points
    putStrLn $ "Fixed points at indices: " ++ show (findFixedPoints p)
    putStrLn "-------------------------------------------"
  where
    formatList = foldr1 (\a b -> a ++ ", " ++ b) . map show
    findFixedPoints perm = [i | (i, val) <- zip [0..10] perm, i == val]
