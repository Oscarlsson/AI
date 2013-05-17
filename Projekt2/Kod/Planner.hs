module Planner where

import PGF
import Data.Maybe
import Shrdlite
import Data.PSQueue as PSQ hiding (null, foldl, foldr)

import qualified Data.Set  as S 
import qualified Data.Map  as M 
import qualified Data.List as L
import qualified NLPParser as P
import Blocks
import ErrM 

import Backend

import Prelude hiding (drop)

initWorld2 = [[], ["a", "b"], ["c", "d"], [], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]]
--initWorld2 = [[], ["a"], ["c","b"], ["d"], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]]
--
--
initialWorld :: World
initialWorld = fromJust $ createWorld initWorld2 "" blocks

--------------------------------------------------------------------------------
--- TEST
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
data Goal = G {goal :: P.Output , blockId :: [Int] }

showHistory :: History -> String
showHistory [] = ""
showHistory (x:xs) = show x ++ ";" ++ showHistory xs

createGoal :: P.Output -> World -> Goal
createGoal p w = G {goal = p, blockId = listofID}
        where
            blocks   = P.mBlocks p
            listofID = map (\b -> fromJust $ M.lookup b (indexes w)) blocks


finished :: World -> Goal -> Bool
-- The implementation of put should be identical to Move as long as
--     b1 is a reference to the holding-block in the initial state.
finished w ( G (P.O P.Put (b1:bs) loc) id) = 
            case loc of
                (P.Beside bs) -> False
                (P.Inside b)  -> False
                (P.LeftOf b)  -> False
                (P.OnTop  b2)  -> isOnTop b2 b1 w
                (P.RightOf b2) -> isRightOf b2 b1 w
                (P.Under b2)   -> isUnder b2 b1 w
                (P.Floor is)  -> False
finished w ( G (P.O P.Move (b1:bs) loc ) _ ) = 
            case loc of
                (P.Beside bs)  -> False--map (\b -> isBeside b1 b w) bs
                (P.Inside (b:bs)) -> isOnTop b1 b w --changed since Inside now takes a list 
                                                    --of possible blocks to put other blocks inside.
                                                    --I guess every such block should be taken in consideration
                                                    --(for now the first block is always taken)
                (P.LeftOf b2)  -> isLeftOf b2 b1 w
                (P.OnTop  b2)  -> isOnTop b2 b1 w
                (P.RightOf b2) -> isRightOf b2 b1 w
                (P.Under b2)   -> isUnder b2 b1 w
                (P.Floor is)   -> isOnPoss b1 (head is) w --Instead of head: closest

finished w ( G (P.O P.Take (b1:bs) _ ) _ ) = maybe False (b1==) (holding w)

finished _ ( G (P.O P.None _ _) _ ) = False

heuristic :: World -> Goal -> Int
heuristic w g 
        | finished w g = 0
        | otherwise = case goal g of
            (P.O P.Take (b1:bs) _) -> 1
            (P.O P.Move (b1:bs) (P.OnTop b2)) -> 
                h1 + h2
                    where
                        holding0 -- Holding something
                            | isNothing (holding w) = 0
                            | otherwise = 1
                        -- To move
                        stackIndex1 = M.lookup b1 (indexes w)
                        stack1 = maybe Nothing (\si -> M.lookup si (ground w)) stackIndex1
                        holding1
                            | isNothing stack1 = 1 -- Holding target, drop it
                            | otherwise = 2 + holding0 -- 2 = pickup/drop target. + holding0 = drop holding
                        stackValue1 = (fromJust $ maybe (Just 0) (L.elemIndex b1) stack1)
                        h1 = holding1 + (2*(stackValue1))
                        -- On top of this
                        stackIndex2 = M.lookup b2 (indexes w)
                        stack2 = maybe Nothing (\si -> M.lookup si (ground w)) stackIndex2
                        holding2
                            | isNothing stack2 = 1 -- Holding target, drop it
                            | otherwise = holding0 -- Drop w/e holding
                        stackValue2 = (fromJust $ maybe (Just 0) (L.elemIndex b2) stack2)
                        h2 = holding2 + (2*(0+stackValue2))

-- Peek ahead 1
heuristic' :: World -> Goal -> Int
heuristic' w g 
    | finished w g = 0
 -- | otherwise = heuristic w g
    | otherwise = 1 + (L.minimum heuristics2)
    where
        successorWorlds = map world $ successors (N w [])
        heuristics2 = map (\s -> heuristic s g) successorWorlds  

--------------------------------------------------------------------------------
-----------------------                                     --------------------
-----------------------             Here be tests           --------------------
-----------------------                                     --------------------
--------------------------------------------------------------------------------
testStatement :: String -> IO ()
testStatement stmt = do
    shrdPGF <- readPGF "Shrdlite.pgf" 
    let o = handleOutput $ head $ P.runParser shrdPGF stmt initialWorld
    let g = createGoal o initialWorld
    let a = astarDebug initialWorld g
    print stmt
    putStrLn $ "\t" ++ ( show $ "Initial heuristic: " ++ (show $ heuristic initialWorld g) )
    putStrLn $ "\t" ++ ( show $ "Nodes visited: " ++ (show $ snd a) )
    putStrLn $ "\t" ++ ( show $ (showHistory (fromJust $ fst a)) )

runTests :: IO ()
runTests = do
    testStatement "take the red square"
    testStatement "take the green pyramid"
    testStatement "put the black wide block on top of the red square"

--command = "put the black block to the left of the green pyramid"
--command = "put the black block to the left of the red square"

handleOutput :: Err P.Output -> P.Output
handleOutput (Ok o) = o
handleOutput (Bad s) = error s

-- This method is just for quickly testing the parser.
printObject :: String -> IO ()
printObject stmt = do
    shrdPGF <- readPGF "Shrdlite.pgf" 
    let o = handleOutput $ head $ P.runParser shrdPGF stmt initialWorld
    print o
--------------------------------------------------------------------------------
-----------------------        \(^v^)/                      --------------------
-----------------------                "No more tests!"     --------------------
--------------------------------------------------------------------------------

data Instruction = Drop Int | Pick Int deriving (Eq)
instance Show Instruction where
    show (Pick l) = "pick " ++ (show l)
    show (Drop l) = "drop " ++ (show l)

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
addAll pq nodes g = foldl (\pq' node -> PSQ.insert node ((length $ history node) + (heuristic' (world node) g)) pq') pq nodes   
						  
successors :: Node -> [Node]
successors (N w h) = nodes
	where 
		moves = allLegalMoves w
		worlds = map (\m -> fromJust $ action m w) moves 
		histories = map (\instr -> h++[instr]) moves
		nodes = map (\t -> (N (fst t) (snd t))) $ zip worlds histories

astarDebug :: World -> Goal -> (Maybe History, Int)
astarDebug w g 
    --- TODO : maybe default (\x -> ) result
	| isNothing (fst result) = (Nothing, 0)
	| otherwise = (Just (history $ fromJust (fst result)), snd result)
	--where result = snd $ astar' (pq w) [] g
	where 
            y = astar' (pq w) [] g
            result = (snd $ y, PSQ.size $ fst y)

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

