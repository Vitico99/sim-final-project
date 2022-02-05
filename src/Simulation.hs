import Agent
import Environment
import System.Random
import Text.Printf
import qualified Data.Set as Set

-- Rows of the room
rows = 5
-- Columns of the room
cols = 5
-- The matrix specification of the room
-- 0 empty
-- 1 obstacle
-- 2 dirt
-- 3 playpen
-- 4 kid
-- 5 robot
-- 6 carrying robot
inputMatrix = [[0,0,1,0,0],[0,3,0,0,5],[0,0,0,0,0],[4,0,1,0,0],[1,0,0,0,1]]
-- maximun time of the simulation
tMax = 100
-- interval of time to randomly mutate the environment
tRnd = 3
-- random seed to use in the simulation
inputSeed = 73
-- the mode of the robot (it's used to select the robot model)
-- mode = 1 stands for the robot which persuits the kids first
-- mode = 2 stands for the robot which persuits the nearest element
mode = 1
-- percent of clean cells to achieve in the simulation
goalPercent = 60
roomSize = rows * cols

percentDirty :: Room -> Float
percentDirty room = let
	dirtCount = Set.size (selectDirts room)
	possibleDirt = roomSize - (Set.size (selectPlaypens room)) - (Set.size (selectObstacles room))
	in percent dirtCount possibleDirt

percent x y = let 
	a = fromIntegral x :: Float
	b = fromIntegral y :: Float
    in 100 * (a / b)


simulate room t gen logger
	| t > tMax = (room, logger, -1)
	| (100 - percentDirty room) >= goalPercent = (room, logger, t)
	| otherwise = let
		info = printf "Starts turn %s" (show t)
		(room', gen', logger') = mutate room t gen (info:logger)
		in simulate room' (t+1) gen' logger'

mutate room t gen logger
	| (t `rem` tRnd) == 0 = let
		(room', logger') = agentsAction room logger mode
		in mutateEnv room' gen True logger'
	| otherwise = let
		(room', logger') = agentsAction room logger mode
		in mutateEnv room' gen False logger'

main = let
	room = fromMatrix inputMatrix
	gen = mkStdGen inputSeed
	logger = ["Started Simulation"]
	(room', logger', result) = simulate room 0 gen logger
	logger'' = reverse logger'
	in print logger''
		-- print result
