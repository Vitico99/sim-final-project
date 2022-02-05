import Geometry
import Control.Exception.Base
import qualified Data.Set as Set

main = let
	-- test the addition and substraction of points
	t1 = assert ((addPoints (1,1) (1,1)) == (2,2)) $ "passed"
	t2 = assert ((subPoints (1,1) (1,1)) == (0,0)) $ "passed"

	-- test the grid method
	answer3 = Set.fromList [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2)]
	result3 = Set.fromList (grid (1,1) 1)
	t3 = assert (Set.isSubsetOf result3 answer3) $ "passed"
	t4 = assert (Set.isSubsetOf answer3 result3) $ "passed"

	-- test adyacents method
	answer5 = [(0,1), (2,1), (1,0), (1,2)]
	result5 = adyacents (1,1)
	t5 = assert (answer5 == result5) $ "passed"

	-- test inBound method
	t6 = assert (inBound (1,1) 0 2 0 2) $ "passed"
	t7 = assert (not (inBound (2,3) 0 2 0 2)) $ "passed"

	-- test BFS
	-- Add some test here
	in print (t1, t2, t3, t4, t5, t6, t7)
