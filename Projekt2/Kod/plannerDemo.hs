import PGF
import Data.Maybe
import Shrdlite
import Data.PSQueue as PSQ hiding (null, foldl, foldr)

import qualified NLPParser as P
import ErrM 
import qualified Blocks as B

type Block = String
type Holding = Maybe Block
type Location = Int

type World = [[Block]]
type State = (Holding, World)

---type Goal = State -- Kan behöva bestå av GF-haskell-typer

type Goal = P.Output 
finished :: State -> Goal -> Bool
finished s (P.O P.Move (b1:bs) (P.LeftOf b2)) = P.isLeftOf (snd s) (b2, b1)

---finished :: State -> Goal -> Bool
---finished s g = s == g

---stateDistance :: State -> Goal -> Int
---stateDistance s g = sum $ map (\tuple -> if (fst tuple == snd tuple) then 0 else 1) (zip (snd s) (snd g))
-- A*
heuristic :: State -> Goal -> Int
---heuristic s g = stateDistance s g
heuristic s g = 1



command :: String 
command = --- "put the green pyramid to the left of the blue tall rectangle"
		  "put the black block to the left of the green pyramid"
main :: IO ()
main = do
	shrdPGF <- readPGF "Shrdlite.pgf" 
	let o = handleOutput $ head $ P.runParser shrdPGF command
	print $ finished initialState o
	print command
	print o
	putStrLn ""
	let s2 = fromJust $ action initialState (Pick 2)
	case o of
		(P.O P.Move (b1:bs) (P.LeftOf b2)) -> do
			print $ P.isLeftOf (snd s2) (b2, b1)
	case o of
		(P.O P.Move (b1:bs) (P.LeftOf b2))
			| P.isLeftOf (snd initialState) (b2, b1) -> do
				print (snd initialState)
				print b1
				print b2
				print "foo"
			| otherwise -> print "foobar"
	print $ astar initialState o


handleOutput :: Err P.Output -> P.Output
handleOutput (Ok o) = o
handleOutput (Bad s) = error "Hoho" 
-- 
data Instruction = Drop Location | Pick Location deriving (Show, Eq)
instance Ord Instruction where
	Pick l1 `compare` Pick l2 = l1 `compare` l2
	Drop l1 `compare` Drop l2 = l1 `compare` l2
	Pick l1 `compare` Drop l2 = l1 `compare` l2
	Drop l1 `compare` Pick l2 = l1 `compare` l2
---instance Eq Instruction where
---	_ == _ = True

initialState :: State
initialState = (Nothing, [[], ["a","b"], ["c","d"], [], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]])

-- Actions
action :: State -> Instruction -> Maybe State
action s@(_, world) i@(Pick location)
	| isPossible s i = Just (Just pickedBlockId, s')
	| otherwise = Nothing
	where	
			pickedBlockId = last $ world!!location
			s' = map (\l -> filter (\b -> b /= pickedBlockId) l) world
action s@(Just holding, world) i@(Drop location) 
	| isPossible s i = Just s'
	| otherwise = Nothing 
	where 
			ix = [0..length world - 1]
			worldWithoutPickedItem = map (\i -> if (i == location) then ((world!!i) ++ [holding]) else (world!!i)) ix
			s' = (Nothing, worldWithoutPickedItem) 

--- Methods for legal moves
legalDrop :: GBlock -> GBlock -> Bool
legalDrop (Gblock _ _ _) (Gblock Gpyramid _ _) = False
legalDrop (Gblock _ _ _) (Gblock Gball _ _) = False
legalDrop (Gblock _ Gsmall _) _ = True 
legalDrop (Gblock _ Gtall _) _ = True 
legalDrop (Gblock _ Gmedium _) (Gblock _ Gmedium _) = True
legalDrop (Gblock _ Gmedium _) (Gblock _ Gwide _) = True
legalDrop (Gblock _ Gmedium _) (Gblock _ Glarge _) = True
legalDrop (Gblock _ Gwide _) (Gblock _ Gwide _) = True
legalDrop (Gblock _ Gwide _) (Gblock _ Glarge _) = True
legalDrop (Gblock _ Glarge _) (Gblock _ Glarge _) = True
legalDrop (Gblock _ Glarge _) (Gblock _ Gwide _) = True
legalDrop (Gblock _ _ _) (Gblock _ _ _) = False

isPossible :: State -> Instruction -> Bool
isPossible (Just _, _) (Pick _) = False
isPossible (Nothing, _) (Drop _) = False
isPossible (_, world) (Pick location) = not $ null $ world!!location 

isPossible s@(_, world) (Drop location) 
	| isNothing stack = True
	| otherwise = legalDrop (fromJust $ blockAtHolding s) (fromJust $ stack)
	where stack = blockAtLocation s location

allLegalMoves :: State -> [Instruction]
allLegalMoves s@(Nothing, world) = 
	filter (\instr -> isPossible s instr) (map (\i -> Pick i) [0..length world - 1])
allLegalMoves s@(Just _, world) = 
	filter (\instr -> isPossible s instr) (map (\i -> Drop i) [0..length world - 1])

--- Methods to retrieve GBlock
blockAtHolding :: State -> Maybe GBlock
blockAtHolding s@(Just a, world) = Just $ tempBlock $ a
blockAtHolding s@(Nothing, world) = Nothing 

blockAtLocation :: State -> Location -> Maybe GBlock
blockAtLocation (holding, world) location 
	| null stack = Nothing
	| otherwise = Just $ tempBlock $ last stack 
	where stack = world!!location 

tempBlock :: String -> GBlock
tempBlock	"a"	=	Gblock	Grectangle	Gtall		Gblue  
tempBlock	"b"	=	Gblock	Gball		Gsmall		Gwhite 
tempBlock	"c"	=	Gblock	Gsquare		Glarge		Gred   
tempBlock	"d"	=	Gblock	Gpyramid	Glarge		Ggreen 
tempBlock	"e"	=	Gblock	Gbox		Glarge		Gwhite 
tempBlock	"f"	=	Gblock	Grectangle	Gwide		Gblack 
tempBlock	"g"	=	Gblock	Grectangle	Gwide		Gblue  
tempBlock	"h"	=	Gblock	Grectangle	Gwide		Gred   
tempBlock	"i"	=	Gblock	Gpyramid	Gmedium		Gyellow
tempBlock	"j"	=	Gblock	Gbox		Glarge		Gred   
tempBlock	"k"	=	Gblock	Gball		Gsmall		Gyellow
tempBlock	"l"	=	Gblock	Gbox		Gmedium		Gred   
tempBlock	"m"	=	Gblock	Gball		Gmedium		Gblue  










type History = [Instruction]
type Node = (State, History)
type PQ = PSQ Node Int
type Seen = [State]

pq :: State -> PQ
pq s = PSQ.singleton (s, []) 0

pop :: PQ -> (Node, PQ)
pop pq = (n,pq')
	where
		n = key $ fromJust $ PSQ.findMin pq
		pq' = PSQ.deleteMin pq

--astaralg :: PQ -> Seen -> PQ -> Seen 
--astraalg = undefined
--astaralg pq s = map pop.insert pq (s') 
--	where 
--		(n,pq') = pop pq 
--		s' = map action (allLegalMoves $ fst n)
addAll :: PQ -> [Node] -> Goal -> PQ
addAll pq nodes g = foldl (\pq' node -> PSQ.insert node ((length $ snd node) + (heuristic (fst node) g)) pq') pq nodes   
						  

successors :: Node -> [Node]
successors n@(s,h) = nodes
	where 
		moves = allLegalMoves s
		states = map (\m -> fromJust $ action s m) moves 
		histories = map (\instr -> h++[instr]) moves
		nodes = zip states histories

astar :: State -> Goal -> Maybe History
astar s g 
	| isNothing result = Nothing
	| otherwise = Just (snd $ fromJust result)
	where result = snd $ astar' (pq s) [] g

astar' :: PQ -> Seen -> Goal -> (PQ, Maybe Node)
astar' pq seen goal 
		| finished (fst n) goal = (pq'', Just n)
--fail  | fail = (pq'', Nothing)
		| otherwise = astar' pq'' seen' goal
		where
			(n,pq') = pop pq
			seen' = (fst n):seen
			succs = filter (\state -> not $ elem (fst state) seen' ) (successors n)
			pq'' = addAll pq' succs goal

	






