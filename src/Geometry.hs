module Geometry
( Pos
, addPoints
, subPoints
, grid
, adyacents
, inBound
, bfs
, getPath
) where

-- This module contain definitions for auxiliar geometric related functions to use in a cartesian coordinate system
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe

type Pos = (Int, Int)

-- Adds two points
-- addPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Substracts two points
-- subPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
subPoints (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- Calculates a grid of points centered at (x, y) that extends to a distance d from center
-- grid :: (Int, Int) -> Int -> List (Int, Int)
grid (x, y) d = [(x', y') | x' <- [x-d..x+d], y' <- [y-d..y+d]]

-- Get the adyacent points to (x, y) in the four principal directions
-- adyacents :: (Int, Int) -> List (Int, Int)
adyacents (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- Returns True if a point (x, y) belongs to a given geometric grid specified by x and y ranges
-- inBound :: (Int, Int) -> Int -> Int -> Int -> Int -> Bool
inBound (x, y) xL xR yL yR = x >= xL && x <= xR && y >= yL && y <= yR

-- An implementation of the BFS algorithm in a cartesian coordinate system 
-- bfs :: List (Int, Int) -> Map (Int, Int) Int -> Map (Int, Int) (Int, Int) -> Set (Int, Int)
-- -> (List (Int, Int), Map (Int, Int) Int, Map (Int, Int) (Int, Int), Set (Int, Int))
--
-- params & return:
-- queue :: List (Int, Int) : The queue of the BFS. In the initial call the queue should contain the starting point of the bfs. In the return it will be empty.
-- distances :: Map (Int, Int) Int : Associates each point with its distance from the starting point. In the initial call this will only contain the key value pair (startPoint, 0)
-- parents :: Map (Int, Int) (Int, Int) : Associates each point with its parent point in the BFS. In the initial call this will only contain the key value pair (startPoint, (-1,-1))
-- visited :: Set (Int, Int) : Contains the visited points by the BFS. In the initial call this will be empty
bfs' :: [Pos] -> Map.Map Pos Int -> Map.Map Pos Pos -> Set.Set Pos -> (Pos -> [Pos]) -> ([Pos], Map.Map Pos Int, Map.Map Pos Pos, Set.Set Pos)
bfs' queue distances parents visited neighbors
	| null queue = (queue, distances, parents, visited)
	| otherwise = let
		point = last queue -- the first in the queue will be the last in the list for efficient insert in the list
		toVisit = [(x,y) | (x,y) <- (neighbors point), Set.notMember (x,y) visited] 
		queue' = toVisit ++ (init queue) -- place the last discovered points at the end of the queue
		distance = fromMaybe 0 (Map.lookup point distances)
		distances' = Map.union distances (Map.fromList [(point', distance+1) | point' <- toVisit]) -- update distances to discovered points
		parents' = Map.union parents (Map.fromList [(point', point) | point' <- toVisit]) -- update parents of discovered points
		visited' = Set.union visited (Set.fromList toVisit) -- update the visited nodes
		in bfs' queue' distances' parents' visited' neighbors

-- Nicer interface method for using the BFS :)
bfs point neighbors = let
	queue = [point]
	distances = Map.singleton point 0
	parents = Map.singleton point (-1,-1)
	visited = Set.empty
	in bfs' queue distances parents visited neighbors

--getPath :: Pos -> Map.Map Pos Pos -> [Pos] -> [Pos]
getPath point parents path = let
	parent = Map.findWithDefault (-1,-1) point parents
	in if parent == (-1,-1) 
		then path
		else getPath parent parents (parent:path)
	
