import Environment
import qualified Data.Set as Set
import Control.Exception.Base
import System.Random
import Agent

tmatrix = [[0,0,1,0,0], [0,3,3,0,5], [0,0,0,0,0], [4,0,1,4,0], [1,0,0,0,1]]

main = let  
		  room = fromMatrix tmatrix
		  (room', logger') = robotAction room (1,4) ["hello"]
		  in print logger'
	
