import Environment
import qualified Data.Set as Set
import Control.Exception.Base
import System.Random

equalSets seta setb = (Set.isSubsetOf seta setb) && (Set.isSubsetOf setb seta)

tmatrix = [[0,0,1,0,0], [0,3,3,0,5], [0,0,0,0,0], [4,0,1,4,0], [1,0,0,0,1]]


testInput matrix = fromMatrix tmatrix


testKidMove1 dst = let 
	empties = Set.fromList [(x,y) | x <- [0..2], y <- [0..2], (x,y) /= (1,1)]
	kids = Set.singleton (1,1)
	room  = (empties, Set.empty, Set.empty, Set.empty, kids, Set.empty, Set.empty)
	-- first case the kid moves to empty cells
	(room', logger') = moveKid room (1,1) dst ["Started simulation"]
	empties' = selectEmpties room'
	kids' = selectKids room'
	exEmpties = Set.fromList [(x,y) | x <- [0..2], y <- [0..2], (x,y) /= dst]
	exKids = Set.singleton dst
	t1 = assert (equalSets empties' exEmpties) $ "Passed"
	t2 = assert (equalSets kids' exKids) $ "Passed"
	in (t1,t2)

simulate room gen logger t
	| t > 0  = let
	rnd = if (t `rem` 3) == 0 then True else False
	(room', gen', logger') = mutate room gen rnd logger
	in simulate room' gen' logger' (t-1)
	| otherwise = (room, gen, logger)


main = let
		gen = mkStdGen 73
		room = testInput tmatrix
		kids = Set.toList (selectKids room)
		--(room', logger') = moveKid room (3,3) (4,3) ["hello"]
		(room', gen', logger') = mutate room gen True ["hello"]
		in print logger'
	
	--print (testKidMove1 (1,2))
	--print (testKidMove1 (1,0))
	--print (testKidMove1 (0,1))
	--print (testKidMove1 (2,1))

