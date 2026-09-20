{-# LANGUAGE BangPatterns #-}

import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.List (foldl')
import Data.Sequence ((|>))

type Perm = [Int]

identity :: Perm
identity = [0..11]

-- Strict element-by-element lookup to avoid memory leaks
multiply :: Perm -> Perm -> Perm
multiply p q = map (\i -> p !! (q !! i)) [0..11]

genA, genB, genC :: Perm
genA = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 11]
genB = [9, 4, 6, 7, 1, 8, 2, 3, 5, 0, 11, 10]
genC = [0, 9, 3, 2, 8, 6, 5, 7, 4, 1, 10, 11]

generators :: [Perm]
generators = [genA, genB, genC]

-- Using Seq.Seq for O(1) push/pop and strict evaluation via BangPatterns
bfs :: Set.Set Perm -> Seq.Seq Perm -> Set.Set Perm
bfs !visited !queue
    | Seq.null queue = visited
    | otherwise =
        let current = Seq.index queue 0
            rest    = Seq.drop 1 queue
            -- Generate and filter next states immediately
            nextPerms = map (\g -> multiply g current) generators
            newPerms  = filter (\p -> not (Set.member p visited)) nextPerms
            -- Force strict insertion into the set
            !updatedVisited = foldl' (\s p -> Set.insert p s) visited newPerms
            -- Append new states to the end of the sequence
            !updatedQueue   = foldl' (|>) rest newPerms
        in bfs updatedVisited updatedQueue

main :: IO ()
main = do
    putStrLn "Calculating the Mathieu group M12..."
    let !totalGroup = bfs (Set.singleton identity) (Seq.singleton identity)

    putStrLn "Printing ALL 95,040 elements... Press Ctrl+C to abort if it takes too long."
    -- mapM_ applys the 'print' function to every single element in the set
    mapM_ print (Set.toList totalGroup)

    let !groupSize = Set.size totalGroup
    putStr "Total number of elements in M12: "
    print groupSize
