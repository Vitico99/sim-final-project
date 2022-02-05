module Environment where 
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import System.Random.Stateful
import Geometry
import Sample
import Text.Printf
import Debug.Trace
-- utils : find place to this methods
replace set old new = Set.insert new (Set.delete old set)

type PosSet = Set.Set Pos
type Room = (PosSet, PosSet, PosSet, PosSet, PosSet, PosSet, PosSet)


selectEmpties (e,_,_,_,_,_,_) = e
selectObstacles (_,o,_,_,_,_,_) = o
selectDirts (_,_,d,_,_,_,_) = d
selectPlaypens (_,_,_,p,_,_,_) = p
selectKids (_,_,_,_,k,_,_) = k
selectRobots (_,_,_,_,_,r,_) = r
selectCRobots (_,_,_,_,_,_,c) = c


cEMPTY = 0
cOBSTACLE = 1
cDIRT = 2
cPLAYPEN = 3
cKID = 4
cROBOT = 5
cCROBOT = 6

----------------------------- Input --------------------------------------------------

fromMatrix matrix = let
	cells = cellsFromMatrix matrix [] 0
	empties = Set.fromList [(x,y) | ((x,y), t) <- cells, t == cEMPTY]
	obstacles = Set.fromList [(x,y) | ((x,y), t) <- cells, t == cOBSTACLE]
	dirts = Set.fromList [(x,y) | ((x,y), t) <- cells, t == cDIRT]
	playpens = Set.fromList [(x,y) | ((x,y), t) <- cells, t == cPLAYPEN]
	kids = Set.fromList [(x,y) | ((x,y), t) <- cells, t == cKID]
	robots = Set.fromList [(x,y) | ((x,y), t) <- cells, t == cROBOT]
	crobots = Set.fromList [(x,y) | ((x,y), t) <- cells, t == cCROBOT]
	in (empties, obstacles, dirts, playpens, kids, robots, crobots)

cellsFromMatrix :: [[Int]] -> [((Int, Int), Int)] -> Int -> [((Int, Int), Int)]
cellsFromMatrix matrix cells nrow
	| null matrix = cells
	| otherwise = let
		cells' = cellsFromMatrix (tail matrix) [] (nrow+1)
		cells = cellsFromRow (head matrix) [] (nrow) 0
		in cells ++ cells'

cellsFromRow :: [Int] -> [((Int, Int), Int)] -> Int -> Int -> [((Int, Int), Int)]
cellsFromRow row cells nrow ncol
	| null row = cells
	| otherwise = let
		cells' = cellsFromRow (tail row) [] nrow (ncol+1)
		in ((nrow, ncol), (head row)) : cells'

----------------------------- State ---------------------------------------------------
-- This section provides functions to get the state of the environment 

freeKids room = let
	kids = selectKids room
	playpens = selectPlaypens room
	in Set.difference kids playpens

existFreeKids room = not (Set.null (freeKids room))

robotInDirt room robot = let
	dirts = selectDirts room
	in Set.member robot dirts

isDirtyRoom room = let
	dirts = selectDirts room
	in not (Set.null dirts)

----------------------------- Mutation definition -------------------------------------

mutateEnv room gen rnd logger
	| rnd == True = let 
		(room', gen', logger') = naturalMutate room gen logger
		in randomMutate room' gen' logger'
	| otherwise = naturalMutate room gen logger

---------------------------- Natural Mutation definition -------------------------------

naturalMutate room gen logger = let
	kids = Set.toList (freeKids room) -- The only kids that can move are the ones that are not in the playpen or carried by a bot
	in moveKids kids room gen logger


moveKids :: [Pos] -> Room -> StdGen -> [String] -> (Room, StdGen, [String])
--moveKids kids room gen logger | trace ("debugg" ++ show logger) False = undefined
moveKids kids room gen logger
	| null kids = (room, gen, logger)
	| otherwise = let
		kid = head kids
		validDsts = Set.union (selectEmpties room) (selectObstacles room)
		possibleDsts = Set.intersection (Set.fromList (adyacents kid)) validDsts
		in if null possibleDsts 
			then moveKids (tail kids) room gen logger
			else let 
				(dst, gen') =  rndElem possibleDsts gen
				(room', logger') = moveKid room kid dst logger
				in moveKids (tail kids) room' gen' logger' -- move the rest of the kids


moveKid :: Room -> Pos -> Pos -> [String] -> (Room, [String])
moveKid (empties, obstacles, dirts, playpens, kids, robots, crobots) src dst logger
	| Set.member dst obstacles = let -- the kid is trying to go to a cell with an obstacle so he will try to move it
		newLastObstacle = findEmpty empties obstacles dst (subPoints dst src)
		in if newLastObstacle == (-1,-1) -- the kid can't move the obstacle so nothing changes
			then let
				info = printf "Kid at %s tries to move the obstacle at %s but fails, stays at %s" (show src) (show dst) (show src)
				in ((empties, obstacles, dirts, playpens, kids, robots, crobots), (info : logger))
			else let
				kids' = replace kids src dst
				obstacles' = replace obstacles dst newLastObstacle
				empties' = replace empties newLastObstacle src	
				info = printf "Kid at %s moves and pushes the obstacle at %s, there is a new obstacle at %s" (show src) (show dst) (show newLastObstacle) 
				in ((empties', obstacles', dirts, playpens, kids', robots, crobots), (info : logger))
	| otherwise = let -- the kid goes to an empty cell
		kids' = replace kids src dst
		empties' = replace empties dst src
		info = printf "Kid at %s moves to %s" (show src) (show dst)
		in ((empties', obstacles, dirts, playpens, kids', robots, crobots), (info : logger))

-- Find an empty space to place the last obstacle when a kid moves a line of obstacles which
-- starts at the point src in the direction dir
findEmpty empties obstacles src dir
	| Set.member src obstacles = findEmpty empties obstacles (addPoints src dir) dir 
	| Set.member src empties = src
	| otherwise = (-1,-1)

------------------------ Random Mutation Definition -------------------------------------

randomMutate room gen logger = dirtRoom room gen logger

dirtRoom room gen logger = let
	(empties, obstacles, dirts, playpens, kids, robots, crobots) = room	
	(newDirt, gen') = kidsGenDirt (Set.toList (freeKids room)) room gen	
	empties' = Set.difference empties newDirt
	dirts' = Set.union dirts newDirt
	info = printf "The kids poop and now positions %s are dirty" (show newDirt)
	in ((empties', obstacles, dirts', playpens, kids, robots, crobots), gen', (info:logger))

kidsGenDirt kids room gen
	| null kids = (Set.empty, gen)
	| otherwise = let
		(dirt, gen') = kidsGenDirt (tail kids) room gen
		(dirt', gen'') = kidGenDirt (head kids) room gen'
		in ((Set.union dirt dirt'), gen'')

kidGenDirt kid room gen = let
	dirtGrid = Set.fromList (grid kid 1)
	possibleDirt = Set.intersection dirtGrid (selectEmpties room)
	kidsInGrid = Set.size (Set.intersection dirtGrid (selectKids room))
	size 
		| kidsInGrid == 1 = 1
		| kidsInGrid == 2 = 3
		| kidsInGrid >= 3 = 6
	in rndSetSample possibleDirt size gen 

----------------------------------------------------------------------------------------
