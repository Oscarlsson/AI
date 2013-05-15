module Planner where

import PGF
import Data.Maybe
import Shrdlite
import Data.PSQueue as PSQ hiding (null, foldl, foldr)

import qualified Data.Set as S 
import qualified Data.Map as M 
import qualified NLPParser as P
import Blocks
import ErrM 

import Backend

import Prelude hiding (drop)

---initWorld2 = [[], ["a"], ["d","c"], [], ["e","f","i","h","g"], [], [], ["j","k"], [], ["l","m"]]
initWorld2 = [[], ["a", "b"], ["c","d"], [], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]]
initialWorld :: World
initialWorld = fromJust $ createWorld initWorld2 "" blocks

--------------------------------------------------------------------------------
--- TEST
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
type Goal = P.Output 
finished :: World -> Goal -> Bool
finished w (P.O P.Move (b1:bs) (P.LeftOf b2)) = isLeftOf b1 b2 w
---stateDistance :: State -> Goal -> Int
---stateDistance s g = sum $ map (\tuple -> if (fst tuple == snd tuple) then 0 else 1) (zip (snd s) (snd g))
heuristic :: World -> Goal -> Int
---heuristic s g = stateDistance s g
heuristic w g = 1

command :: String
command = "put the black block to the left of the green pyramid"
main :: IO ()
main = do
	shrdPGF <- readPGF "Shrdlite.pgf" 
	let o = handleOutput $ head $ P.runParser shrdPGF command
	print $ finished initialWorld o
	print command
	print o
	putStrLn ""
	let w2 = fromJust $ action (Pick 2) initialWorld 
	case o of
		(P.O P.Move (b1:bs) (P.LeftOf b2)) -> do
			print $ isLeftOf b2 b1 w2
	case o of
		(P.O P.Move (b1:bs) (P.LeftOf b2))
			| isLeftOf b2 b1 initialWorld -> do
				print initialWorld
				print b1
				print b2
				print "foo"
			| otherwise -> print "foobar"
	print $ astar initialWorld o

handleOutput :: Err P.Output -> P.Output
handleOutput (Ok o) = o
handleOutput (Bad s) = error "Hoho" 
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Instruction = Drop Int | Pick Int deriving (Show, Eq)
instance Ord Instruction where
	Pick l1 `compare` Pick l2 = l1 `compare` l2
	Drop l1 `compare` Drop l2 = l1 `compare` l2
	Pick l1 `compare` Drop l2 = l1 `compare` l2
	Drop l1 `compare` Pick l2 = l1 `compare` l2

--------------------------------------------------------------------------------

allLegalMoves :: World -> [Instruction]
allLegalMoves w
				| isJust $ holding w = filter (\instr -> validInstruction instr w) (map Drop (M.keys (ground w)))
				| otherwise = filter (\instr -> validInstruction instr w) (map Pick (M.keys (ground w)))

--------------------------------------------------------------------------------

validInstruction :: Instruction -> World -> Bool
validInstruction i w = case i of 
				Pick x -> validPickId x w 
				Drop x -> validDrop x w 

validPickId :: Int -> World -> Bool
validPickId i w 
			  | isJust $ holding w = False
			  | otherwise = case M.lookup i (ground w) of 
					Nothing		-> False
					Just []		-> False  
					Just (x:xs) -> True

validPick :: Block -> World -> Bool 
validPick b w | isJust $ holding w = False
              | otherwise = 
                case M.lookup b (indexes w) of 
                 Nothing -> False  
                 Just i  -> case M.lookup i (ground w) of 
                                    Nothing     -> False 
                                    Just (x:xs) -> x == b 
                                    Just []     -> False  

validDrop :: Int -> World -> Bool
validDrop i w = case holding w  of 
                    Nothing -> False 
                    Just holdBlock  -> case M.lookup i (ground w) of 
                                         Just []     -> True
                                         Just (groundBlock:xs) -> groundBlock <= holdBlock
                                         Nothing     -> False   

--------------------------------------------------------------------------------

action :: Instruction -> World -> Maybe World
action i w
            | not $ validInstruction i w = Nothing
            | otherwise = case i of
                (Drop x) -> drop x w
                (Pick x) -> pickId x w

pickId :: Int -> World -> Maybe World
pickId i w = case M.lookup i (ground w) of
                Nothing -> Nothing
                Just [] -> Nothing
                Just (b:bs) -> pick b w

pick :: Block -> World -> Maybe World 
pick b w | validPick b w = return $ w {holding = Just b, ground = ground $ newWorld, indexes = indexes newWorld} 
         | otherwise     = Nothing 
            where newWorld = deleteBlock b w  

deleteBlock :: Block -> World -> World 
deleteBlock b w = w {ground = M.update (return . tail) i (ground w), indexes = M.delete b (indexes w)}  
        where i = fromJust $ M.lookup b (indexes w)

drop :: Int -> World -> Maybe World 
drop i w | validDrop i w = return $ w {holding = Nothing, ground = ground $ newWorld, indexes = indexes newWorld}
         | otherwise = Nothing 
            where newWorld = addBlock i (fromJust $ holding w) w

addBlock :: Int -> Block -> World -> World 
addBlock i b w = w {indexes = M.insert b i (indexes w), ground = M.update (return . (b :)) i (ground w)} 
        --where i = fromJust $ M.lookup b (indexes w)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type History = [Instruction]
---type Node = (World, History)
data Node = N { world :: World , history :: History } deriving (Eq, Ord, Show)
type PQ = PSQ Node Int
type Seen = [World]

pq :: World -> PQ
pq w = PSQ.singleton (N {world = w, history = []}) 0

pop :: PQ -> (Node, PQ)
pop pq = (n,pq')
	where
		n = key $ fromJust $ PSQ.findMin pq
		pq' = PSQ.deleteMin pq

addAll :: PQ -> [Node] -> Goal -> PQ
addAll pq nodes g = foldl (\pq' node -> PSQ.insert node ((length $ history node) + (heuristic (world node) g)) pq') pq nodes   
						  
successors :: Node -> [Node]
successors (N w h) = nodes
	where 
		moves = allLegalMoves w
		worlds = map (\m -> fromJust $ action m w) moves 
		histories = map (\instr -> h++[instr]) moves
		nodes = map (\t -> (N (fst t) (snd t))) $ zip worlds histories

astar :: World -> Goal -> Maybe History
astar w g 
    --- TODO : maybe default (\x -> ) result
	| isNothing result = Nothing
	| otherwise = Just (history $ fromJust result)
	where result = snd $ astar' (pq w) [] g

astar' :: PQ -> Seen -> Goal -> (PQ, Maybe Node)
astar' pq seen goal 
		| finished (world n) goal = (pq'', Just n)
--fail  | fail = (pq'', Nothing)
		| otherwise = astar' pq'' seen' goal
		where
			(n,pq') = pop pq
			seen' = (world n):seen
			succs = filter (\n -> not $ elem (world n) seen' ) (successors n)
			pq'' = addAll pq' succs goal

