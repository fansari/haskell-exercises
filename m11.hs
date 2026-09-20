module Main where

import qualified Data.Set as Set

type Permutation = [Int]

identity :: Permutation
identity = [0..10]

-------------------------------------------------------------------------------
-- 1. Official GAP Generators for M_11 (0-based)
-------------------------------------------------------------------------------
genA :: Permutation
genB :: Permutation
genA = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0]
-- genB = [0, 1, 6, 9, 5, 3, 10, 2, 8, 4, 7]
-- genB = [8 ,4, 6, 3, 1, 5, 2, 10, 0, 9, 7]
-- genB = [3, 1, 9, 0, 7, 8, 6, 4, 5, 2, 10]
-- genB = [10, 1, 5, 9, 8, 2, 6, 7, 4, 3, 0]
genB = [4, 1, 2, 6, 0, 10, 3, 7, 9, 8, 5]

buildGenB :: Permutation
buildGenB = map target [0..10]
  where
    target 2  = 6
    target 6  = 10
    target 10 = 7
    target 7  = 2
    target 3  = 9
    target 9  = 4
    target 4  = 5
    target 5  = 3
    target x  = x

-------------------------------------------------------------------------------
-- 2. Permutation Composition: (p1 ∘ p2)(i) = p1(p2(i))
-------------------------------------------------------------------------------
multiply :: Permutation -> Permutation -> Permutation
multiply p1 p2 = map (\i -> p1 !! (p2 !! i)) [0..10]

-------------------------------------------------------------------------------
-- 3. Strict Evaluation Helper (Top-Level Scope)
-------------------------------------------------------------------------------
forceList :: [Permutation] -> a -> a
forceList [] r = r
forceList (p:ps) r = length p `seq` forceList ps r

-------------------------------------------------------------------------------
-- 4. Honest Layer-based Generation (No artificial boundaries)
-------------------------------------------------------------------------------
generateM11 :: [Permutation]
generateM11 = Set.toList $ generateLayers (Set.singleton identity) [identity]
  where
    generateLayers visited [] = visited
    generateLayers visited currentLayer =
      let b = genB
          nextElements     = concatMap (\p -> [multiply p genA, multiply p b]) currentLayer
          nextSet          = Set.fromList nextElements

          newDiscoveredSet = Set.difference nextSet visited
          newDiscovered    = Set.toList newDiscoveredSet
          updatedVisited   = Set.union visited newDiscoveredSet

      in forceList newDiscovered $
         if Set.null newDiscoveredSet
            then visited
            else generateLayers updatedVisited newDiscovered

-------------------------------------------------------------------------------
-- 5. Main with element output
-------------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "Calculating M_11..."
    let group = generateM11
    let n = length group

    putStrLn "Elements:"

    -- Using mapM_ to execute the IO action for each element.
    mapM_ (putStrLn . formatPermutation) group
    putStrLn "--------------------------------------------------------"
    putStrLn $ "Total number of elements in the Mathieu group M11: " ++ show n
    putStrLn "--------------------------------------------------------"

-- Helper function for better readability
formatPermutation :: Permutation -> String
formatPermutation p = "[" ++ foldr1 (\a b -> a ++ ", " ++ b) (map show p) ++ "]"
