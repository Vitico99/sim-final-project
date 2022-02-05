module Agent where
import Environment
import Geometry
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Text.Printf
import Data.List
import Debug.Trace

maxDistance = 100000

agentsAction room logger mode = let
	robots = selectRobots room
	crobots = selectCRobots room
	agents = Set.toList (Set.union robots crobots)
	in agentsAct agents room logger mode

agentsAct agents room logger mode
	| null agents = (room, logger)
	| otherwise = let
		(room', logger') = agentsAct (tail agents) room logger mode
		in if mode == 1 then robotAction room' (head agents) logger'
			else robotAction2 room' (head agents) logger'

--robotAction room robot logger | trace ("debugg" ++ show room) False = undefined
robotAction room robot logger
	| carryKid room robot = leaveInPlaypen room robot logger
	| existFreeKids room = persuitNearestKid room robot logger
	| robotInDirt room robot = cleanDirt room robot logger
	| isDirtyRoom room = persuitNearestDirt room robot logger
	| otherwise = dontMove room robot logger

robotAction2 room robot logger
	| carryKid room robot = leaveInPlaypen room robot logger
	| robotInDirt room robot = cleanDirt room robot logger
	| otherwise = persuitNearestTarget room robot logger 

--------------------------------- Robot BFS --------------------------------------------
-- This function returns a lambda function to get the points to visit next by the robot
robotPossibleDsts (empties, obstacles, dirts, playpens, kids, robots, crobots) = let
	s = Set.union (Set.union (Set.union dirts kids) empties) playpens
	s' = Set.union robots crobots
	validDsts = Set.difference s s'
	in \robot -> Set.toList (Set.intersection (Set.fromList (adyacents robot)) validDsts)

crobotPossibleDsts (empties, obstacles, dirts, playpens, kids, robots, crobots) = let
	s = Set.union (Set.union dirts empties) playpens
	s' = Set.union (Set.union robots crobots) kids
	validDsts = Set.difference s s'
	in \robot -> Set.toList (Set.intersection (Set.fromList (adyacents robot)) validDsts)

----------------------------------------------------------------------------------------
sortBySecond (_,a) (_,b) 
	| a < b = LT
	| otherwise = GT

carryKid room robot = Set.member robot (selectCRobots room)



moveRobot (empties, obstacles, dirts, playpens, kids, robots, crobots) src dst logger
	| Set.member src crobots = let
		-- empties': if the robot is sharing a cell with dirt or playpen then the cell is not empty after the robot moves
		empties' = if (Set.member src dirts) || (Set.member src playpens)
			then empties
			else Set.insert src empties
		-- empties'': if the robot moves to a empty then remove it from the set
		empties'' = if Set.member dst empties' then Set.delete dst empties' else empties'
		crobots' = Set.insert dst (Set.delete src crobots)
		info = printf "Robot carrying kid moves from %s to %s" (show src) (show dst)
		in ((empties'', obstacles, dirts, playpens, kids, robots, crobots'), (info:logger))
		
	| otherwise = let
		-- empties': if the robot is sharing a cell with dirt, kid o playpen then the cell is not empty after the robot moves
		empties' = if (Set.member src dirts) || (Set.member src kids) || (Set.member src playpens)
			then empties
			else Set.insert src empties
		-- empties'': if the robot moves to a empty then remove it from the set
		empties'' = if Set.member dst empties' then Set.delete dst empties' else empties'
		
		kids' = if Set.member dst kids then Set.delete dst kids else kids
		robots' = if Set.member dst kids then Set.delete src robots else Set.insert dst (Set.delete src robots)
		crobots' = if Set.member dst kids then Set.insert dst crobots else crobots
		info = if Set.member dst kids 
			then printf "Robot %s moves to %s and picks up a kid" (show src) (show dst)
			else printf "Robot %s moves to %s" (show src) (show dst)

		in ((empties'', obstacles, dirts, playpens, kids', robots', crobots'), (info:logger))

cleanDirt (empties, obstacles, dirts, playpens, kids, robots, crobots) robot logger = let
	dirts' = Set.delete robot dirts
	info = printf "Robot %s cleans the dirt of its cell" (show robot)
	in ((empties, obstacles, dirts', playpens, kids, robots, crobots), (info:logger))

dontMove room robot logger = let
	info = printf "Robot %s does not move" (show robot)
	in (room, (info:logger))

-- Returns the nearest element to the robot from a collection of elements
-- It will return a tuple of 3 elements:
-- The target of the robot
-- The distance to the target
-- The next cell in the path to the target
--getNearest :: [Pos] -> Room -> Pos ->  -> (Pos, Int, Pos)
getNearest elems room robot possibleDsts = let
	(_, distances, parents, visited) = bfs robot (possibleDsts room) -- get distances and parents to all reachable cells 
	possibleTargets = [((x,y), d) | (x,y) <- elems, let d = (Map.findWithDefault maxDistance (x,y) distances)]
	(point, distance) = head (sortBy sortBySecond possibleTargets)
	path = getPath point parents [point]
	destination = path !! 1
	in (point, distance, destination)

persuitNearestKid room robot logger = let
	kids = Set.toList (freeKids room)
	(target, distance, destination) = getNearest kids room robot robotPossibleDsts
	info = printf "Robot at %s locks target at kid %s" (show robot) (show target)
	in moveRobot room robot destination (info:logger)

persuitNearestDirt room robot logger = let
	dirts = Set.toList (selectDirts room)
	(target, distance, destination) = getNearest dirts room robot robotPossibleDsts
	info = printf "Robot at %s locks target at dirt %s" (show robot) (show target)
	in moveRobot room robot destination (info:logger)

persuitNearestPlaypen room robot logger = let
	playpens = selectPlaypens room
	kids = selectKids room 
	emptyPlaypens = Set.toList (Set.difference playpens kids)
	(target, distance, destination) = getNearest emptyPlaypens room robot crobotPossibleDsts
	info = printf "Robot at %s locks target at playpen %s" (show robot) (show target)
	(room', logger') = moveRobot room robot destination (info:logger) 
	in (room', logger', destination) 

dropKid (empties, obstacles, dirts, playpens, kids, robots, crobots) robot logger = let
	robots' = Set.insert robot robots
	kids' = Set.insert robot kids
	crobots' = Set.delete robot crobots
	info = printf "Robot droped kid at %s" (show robot)
	in ((empties, obstacles, dirts, playpens, kids', robots', crobots'), (info:logger))

leaveInPlaypen room robot logger 
	| robotInPlaypen room robot = dropKid room robot logger
	| otherwise = let
		(room', logger', robot') = persuitNearestPlaypen room robot logger
		in if robotInPlaypen room' robot'
			then (room', logger')
			else let
				(room'', logger'', robot'') = persuitNearestPlaypen room' robot' logger'
				in (room'', logger'')

robotInPlaypen room robot = Set.member robot (selectPlaypens room)

persuitNearestTarget room robot logger = let
	(kid, kidDist, kidDst)
		| existFreeKids room = let
			kids = Set.toList (freeKids room)
			in getNearest kids room robot robotPossibleDsts
		| otherwise = ((-1,-1), maxDistance, (-1,-1))

	(dirt, dirtDist, dirtDst)
		| isDirtyRoom room = let
			dirts = Set.toList (selectDirts room)
			in getNearest dirts room robot robotPossibleDsts
		| otherwise = ((-1,-1), maxDistance, (-1,-1))

	in if kidDist < dirtDist 
		  then persuitNearestKid room robot logger
	   else if dirtDist > kidDist
		  then persuitNearestDirt room robot logger
	   else if kidDist /= maxDistance 
	      then persuitNearestKid room robot logger
	   else dontMove room robot logger
