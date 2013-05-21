module Planner where

import Data.Maybe
import Data.PSQueue as PSQ hiding (null, foldl, foldr)

--import qualified Data.Set  as S 
import qualified Data.Map  as M 
import qualified Data.List as L
import qualified NLPParser as P
import Blocks
import ErrM 

import Backend

import Prelude hiding (drop)


data Goal = G {goal :: P.Output , blockId :: [Int] }

createGoal :: P.Output -> World -> Goal
createGoal p w = G {goal = p, blockId = listofID}
        where
            blocks   = P.mBlocks p
            listofID = map (\b -> fromJust $ M.lookup b (indexes w)) blocks

finished :: World -> Goal -> Bool
finished w ( G (P.O P.Take (b1:bs) _ ) _ ) = maybe False (b1==) (holding w)
finished _ ( G (P.O P.None _ _) _ ) = False
finished w ( G (P.O _ b1S@(b1:b1s) loc ) _ ) = 
            case loc of
                --(P.Location P.RightOf (b2:bs))  -> isRightOf b2 b1 w
                (P.Location P.LeftOf (b2:b2s))  -> and $ map (\b -> isLeftOf b b2 w) b1S
                --(P.Location P.Beside (b2:b2s))  -> False--map (\b -> isBeside b1 b w) bs
                (P.Floor _)                     -> isOnBottom b1 w
                (P.Location P.Inside (b2:b2s))  -> 
                        case b1s of 
                            [] -> isOnTop' b1 b2 w 
                            _ -> and $ map (\b1x -> isAbove b1x b2 w) b1S
                (P.Location P.OnTop (b2:_))     -> 
                    case b1s of 
                        [] -> isOnTop' b1 b2 w
                        _  -> allAbove && oneOnTop
                            where 
                                allAbove = and $ map (\b1x -> isAbove b1x b2 w) b1S
                                oneOnTop = or $ map (\b1x -> isOnTop' b1x b2 w) b1S
                (P.Location P.Under (b2:bs))    -> isUnder b1 b2 w

obstructingBlocks :: [Block] -> Block -> (Block -> Block -> World -> Bool) -> World -> Int
obstructingBlocks mblocks target premise w = sum $ map (\m -> length $ filter (\b -> (not $ b `elem` mblocks) && (isAbove b m w) && (not $ premise m target w)) (blocksInSameStack m w)) mblocks

heuristic :: World -> Goal -> Int
heuristic w g 
        | finished w g = 0
        | otherwise = case goal g of                            -- 1 is just temporary
            ( P.O P.Take    mblocks     _                           ) -> obstructingBlocks mblocks NullBlock (\_ _ _ -> True) w
            ( P.O _          _          (P.Empty)                   ) -> 1
            ( P.O _         mblocks     (P.Floor is)                ) -> hObject + hTarget + hObjectsOutOfPlace + holdingObjectAdjustment
                        where
                        --  hObject = (*) 2 $ sum $ map (\m -> length $ filter (\b -> (not $ b `elem` mblocks) && (isAbove b m w) && (not $ isOnBottom m w)) (blocksInSameStack m w)) mblocks -- TODO: Test & comment
                            hObject = 2 * obstructingBlocks mblocks NullBlock (\m _ w -> isOnBottom m w) w
                            hTarget = 2*(getMinimumStackHeight w)
                            hObjectsOutOfPlace = (*) 2 $ length $ filter (\m -> not $ isOnBottom m w) mblocks 
                            -- TODO TODO TODO TODO These methods look horrible, FIX
                            holdingObjectAdjustment = (\c -> if c then -1 else 0) (or $ map (\m -> isHolding m w) mblocks)

            ( P.O _         mblocks     (P.Location loc tS@(t:_))   ) -> -- tS är ALTERNATIVE TARGETS
                case loc of
                    P.Above     -> 1 
                    P.RightOf   -> 1
                    P.Beside    -> 1
                    P.LeftOf 
                        | isHolding t w -> maxBound - aStarTimeout -- Just some arbitrary large value.
                        -- | otherwise     -> error $ "foo" ++ show hObject --hTarget + hObject
                        | otherwise     -> hTarget + hObject + hObjectsOutOfPlace + holdingObjectAdjustment + holdingSomethingOtherThanTargetAdjustment
                        where
                            -- hObject = (*) 2 $ sum $ map (\m -> length $ filter (\b -> (not $ b `elem` mblocks) && (isAbove b m w) && (not $ isLeftOf m t w))  (blocksInSameStack m w)) mblocks -- TODO: Test & comment
                            hObject = 2 * obstructingBlocks mblocks t isLeftOf w
                            hTarget = case map (\stack -> (*) 2 $ length $ filter (\b -> or $ map ((<) b) mblocks) stack) (take (fromJust $ getBlockIndex t w) $ M.elems $ ground w) of
                                [] -> 0 -- No possible solution; will timeout...
                                xs -> minimum xs
                            hObjectsOutOfPlace = (*) 2 $ length $ filter (\m -> not $ isLeftOf m t w) mblocks 
                            holdingObjectAdjustment = (\c -> if c then -1 else 0) (or $ map (\m -> isLeftOf m t w) mblocks)
                            holdingSomethingOtherThanTargetAdjustment = (\c -> if c then 1 else 0) ((isJust $ holding w) && (not $ or $ map (\m -> isHolding m w) mblocks))

                    P.OnTop     -> hObject + hTarget + hObjectsOutOfPlace + holdingObjectAdjustment + holdingSomethingOtherThanTargetAdjustment
                        where
                        --  hObject = (*) 2 $ sum $ map (\m -> length $ filter (\b -> (not $ b `elem` mblocks) && (isAbove b m w) && (not $ isAbove m t w)) (blocksInSameStack m w)) mblocks -- TODO: Test & comment
                            hObject = 2 * obstructingBlocks mblocks t isAbove w
                            hTarget = (*) 2 $ length $ filter (\b -> (not $ b `elem` mblocks) && (isOnTop' b t w)) (blocksInSameStack t w) -- TODO: Test & comment
                            hObjectsOutOfPlace 
                                | hTarget == 0  = (*) 2 $ length $ filter (\m -> not $ isAbove m t w) mblocks
                                | otherwise     = (*) 2 $ length $ mblocks -- Possible tweak.
                            hTargetsOutOfPlace = 0 -- Possible tweak: Check isOnPoss (original position, has not moved)
                            -- TODO TODO TODO TODO These methods look horrible, FIX
                            holdingObjectAdjustment = (\c -> if c then -1 else 0) (or $ map (\m -> isHolding m w) mblocks)
                            holdingSomethingOtherThanTargetAdjustment = (\c -> if c then 1 else 0) ((isJust $ holding w) && (not $ or $ map (\m -> isHolding m w) mblocks))
                    P.Inside     -> hObject + hTarget + hObjectsOutOfPlace + holdingObjectAdjustment + holdingSomethingOtherThanTargetAdjustment
                        where
                         -- hObject = (*) 2 $ sum $ map (\m -> length $ filter (\b -> (not $ b `elem` mblocks) && (isAbove b m w) && (not $ isAbove m t w)) (blocksInSameStack m w)) mblocks -- TODO: Test & comment
                            hObject = 2 * obstructingBlocks mblocks t isAbove w
                                                              ---------------------------------
                                                              ---- Mindre än något mblock -----
                            hTarget = (*) 2 $ length $ filter (\b -> or $ map ((<) b) mblocks) (blocksInSameStack t w)
                            hObjectsOutOfPlace 
                                | hTarget == 0  = (*) 2 $ length $ filter (\m -> not $ isAbove m t w) mblocks
                                | otherwise     = (*) 2 $ length $ mblocks -- Possible tweak.
                            hTargetsOutOfPlace = 0 -- Possible tweak: Check isOnPoss (original position, has not moved)
                            -- TODO TODO TODO TODO These methods look horrible, FIX
                            holdingObjectAdjustment = (\c -> if c then -1 else 0) (or $ map (\m -> isHolding m w) mblocks)
                            holdingSomethingOtherThanTargetAdjustment = (\c -> if c then 1 else 0) ((isJust $ holding w) && (not $ or $ map (\m -> isHolding m w) mblocks))
                    P.Under     -> hObject + hTarget
                        where
                            hObject = 2 * obstructingBlocks mblocks t isUnder w
                            hTarget = (*) 2 $ length $ filter (\x -> isAbove x t w) (blocksInSameStack t w)
--                          hObjectsOutOfPlace 
--                              | hTarget == 0  = (*) 2 $ length $ filter (\m -> not $ isAbove m t w) mblocks
--                              | otherwise     = (*) 2 $ length $ mblocks -- Possible tweak.

-- |Returns the list of blocks inside the same stack as the queried block. Assumes that the block is not in holding.
blocksInSameStack :: Block -> World -> [Block]
blocksInSameStack b w 
    | blockIndex /= Nothing = fromJust $ getBlocksAt (fromJust $ blockIndex) w
    | otherwise = []
        where blockIndex = getBlockIndex b w 

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
                    Nothing     -> False
                    Just []     -> False  
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
                                         --Just (groundBlock:xs) -> groundBlock <= holdBlock
                                         Just (groundBlock:xs) -> not $ groundBlock < holdBlock
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

aStarTimeout :: Int
aStarTimeout = 1000

astarDebug :: World -> Goal -> (Err History, Int)
astarDebug w g = 
    case fst result of 
        Bad s -> (Bad s , 0)
        Ok  n -> (Ok (history $ n), snd result)

    --- TODO : maybe default (\x -> ) result
    -- | isNothing (fst result) = (Nothing, 0)
   --  | otherwise = (Just (history $ fromJust (fst result)), snd result)
    --where result = snd $ astar' (pq w) [] g
    where 
            y = astar' aStarTimeout (pq w) [] g
            result = (snd $ y, PSQ.size $ fst y)

astar :: Int -> World -> Goal -> Err History
astar to w g =  
    --- TODO : maybe default (\x -> ) result
    case result of 
        Bad s  -> Bad s 
        Ok n   -> Ok (history $ n)
    where result = snd $ astar' to (pq w) [] g

astar' :: Int -> PQ -> Seen -> Goal -> (PQ, Err Node)
astar' to pq seen goal 
        | finished (world n) goal = (pq'', Ok n)
        -- | to == 0  = (pq'',Ok n)
        | to == 0   = (pq'', Bad $ "Impossible command. Last visited world: " ++ (show $ world n))
        | otherwise = astar' (to - 1) pq'' seen' goal
        where
            (n,pq') = pop pq
            seen' = (world n):seen
            succs = filter (\n -> not $ elem (world n) seen' ) (successors n)
            pq'' = addAll pq' succs goal
