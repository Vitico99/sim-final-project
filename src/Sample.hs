module Sample
( rndElem
, rndSetSample
) where

-- This module contains useful functions to random sample sets
import qualified Data.Set as Set
import System.Random.Stateful

-- Samples a random element from a set
-- rndElem :: Set T -> rndGen -> ( T, rdnGen)
rndElem :: Set.Set a -> StdGen -> (a, StdGen)
rndElem set gen = let
	(index, gen') = randomR (0 :: Int, ((Set.size set) - 1) :: Int) gen
	elem = Set.elemAt index set
	in (elem, gen')

-- Returns a subset of set obtained by random sampling set sample times
-- rndSetSample :: Set T -> Int -> rndGen -> (Set T, rndGen)
rndSetSample set samples gen
	| null set = (Set.empty, gen)
	| samples > 0 = let
		(set', gen') = rndSetSample set (samples-1) gen
		(index, gen'') = uniformR (0 :: Int, ((Set.size set) - 1) :: Int) gen'
		set'' = Set.insert (Set.elemAt index set) set'
		in (set'', gen'')
	| otherwise = (Set.empty, gen)

		
--addDirt (empties, obstacles, dirts, playpens, kids, robots, crobots) oldKids =
--	newDirts = 
genRandomSet :: RandomGen g => Int -> Int -> Int -> g -> (Set.Set Int, g)
genRandomSet a b samples gen
	| samples > 0 = let
		(set, gen') = genRandomSet a b (samples-1) gen
		(rand, gen'') = uniformR (a :: Int, b :: Int) gen'
		in (Set.insert rand set, gen'')
	| otherwise = (Set.empty, gen) 
