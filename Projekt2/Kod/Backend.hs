module Backend where 

import PGF
import Blocks
import qualified Data.Set as S 
import qualified Data.Map as M 
import Control.Monad.State
import Data.Maybe 
import Data.List hiding (drop)
import Data.Tuple (swap) 

-- |The key is the x-coordinate and the value is the blocks at this index
type Ground  = M.Map Int [Block]   
-- |The key is a block in the world and the value the x-coordinate of this block 
type Indexes = M.Map Block Int 

data World = W {holding :: Maybe Block , ground :: Ground, indexes :: Indexes, wsize :: Int}
    deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

-- |Takes the name of a block and returns this block if it exist in the world 
findBlockByName :: String -> World -> Maybe Block 
findBlockByName str w | isJust maybeBlock = maybeBlock 
                      | otherwise = if liftM name (holding w) == Just str then holding w else Nothing    
                where maybeBlock = liftM fst . find (\(b,_) -> name b == str) $ M.toList (indexes w)  

--------------------------------------------------------------------------------

-- |Check if the first input block is to the left of the second input block 
isLeftOf :: Block -> Block -> World -> Bool 
isLeftOf b1 b2 w = isOnGround b1 w && isOnGround b2 w 
                && fromJust (liftM2 (<)  (M.lookup b1 (indexes w)) (M.lookup b2 (indexes w)))  

-- |Check if the first input block is to the right of the second input block 
isRightOf :: Block -> Block -> World -> Bool 
isRightOf b1 b2 w = isOnGround b1 w && isOnGround b2 w 
                && fromJust (liftM2 (>)  (M.lookup b1 (indexes w)) (M.lookup b2 (indexes w)))    

-- |Check if the first input block is above the second input block 
isAbove :: Block -> Block -> World -> Bool 
isAbove b1 b2 w = isOnGround b1 w && isOnGround b2 w && b2 `elem` xs && 
                fromJust (liftM2 (<)  (findIndex (==b1) xs) (findIndex (==b2) xs))   
            where xs = fromJust $ M.lookup  (fromJust $ M.lookup b1 (indexes w)) (ground w) 

-- |Check if the first input block is under the second input block 
isUnder :: Block -> Block -> World -> Bool 
isUnder b1 b2 w = isOnGround b1 w && isOnGround b2 w && b2 `elem` xs && 
                fromJust (liftM2 (>)  (findIndex (==b1) xs) (findIndex (==b2) xs))   
            where xs = fromJust $ M.lookup  (fromJust $ M.lookup b1 (indexes w)) (ground w) 

-- |check if a block is on the ground in a world or equally the block exists in the world and 
--  is not the current holding 
isOnGround :: Block -> World -> Bool 
isOnGround b w = M.member b $ indexes w   

-- |Check if a block is on the top of any stack in a world     
isOnTop :: Block -> Block -> World -> Bool 
isOnTop b1 b2 w = liftM2 (==) mi1 mi2 == Just True && (head . fromJust $ M.lookup (fromJust mi1) (ground w)) == b2 
            where (mi1, mi2) = (getBlockIndex b1 w, getBlockIndex b2 w) 

-- |Check if the first input block lies directly over the second input block 
isOnTop' :: Block -> Block -> World -> Bool
isOnTop' b1 b2 w = sameStack && ontop
    where   s1 = M.lookup b1 (indexes w)
            s2 = M.lookup b2 (indexes w)
            sameStack = s1 == s2 -- Both cannot be Nothing
            stack = fromJust $ M.lookup (fromJust s1) (ground w)
            i1 = fromJust $ elemIndex b1 stack
            i2 = fromJust $ elemIndex b2 stack
            ontop = i1 == (i2 - 1)


-- |Check if a block is at the bottom of any stack in a world     
isOnBottom :: Block -> World -> Bool 
isOnBottom b w | isJust mi = (last . fromJust $ M.lookup (fromJust mi) (ground w)) == b
               | otherwise = False  
            where mi = getBlockIndex b w 
    
-- |Check if the first input block is either left or right of secondond input block in a given world 
isBeside :: Block -> Block -> World -> Bool 
isBeside b1 b2 w = isLeftOf b1 b2 w || isRightOf b1 b2 w
   
-- |Check if a block is a holding in the world 
isHolding :: Block -> World -> Bool
isHolding b w = liftM (== name b) (liftM name (holding w)) == Just True  

-- |Check if a there exist blocks on a given x-coordinate in world 
isEmptyIndex :: Int -> World -> Bool 
isEmptyIndex i w = case M.lookup i (ground w) of 
                        Just [] -> True
                        _       -> False  


-- |check if a x-coordinate holds the input block  
isOnPoss :: Block -> Int -> World -> Bool
isOnPoss b i w = i ==  (fromJust $ getBlockIndex b w)

createWorld :: [[String]] -> String -> [Block] -> Maybe World
createWorld ss "" bs = case (createGround ss bs , createIndexes ss bs) of 
                            (Just gr,Just ind) -> Just $ W {ground = gr , indexes = ind, 
                                                            holding = Nothing, wsize = length ss}
                            _                  -> Nothing 
-- |Create a world 
createWorld ss hol bs = case (createGround ss bs , createIndexes ss bs,getBlock hol bs) of 
                            (Just gr,Just ind, Just hol') -> 
                                Just $ W {ground = gr , indexes = ind, holding = Just hol', wsize = length ss}
                            _                             -> Nothing 

-- |Used by createWorld
createGround :: [[String]] -> [Block] -> Maybe Ground
createGround ss bs = liftM M.fromList . sequence $ map (\(i,s) ->createBlocks' i s bs) ([0 .. ] `zip` ss)

-- |Used by createWorld
createIndexes :: [[String]] -> [Block] -> Maybe Indexes 
createIndexes ss bs = liftM (M.fromList . concat) . sequence $ map (\(i,s) ->createBlocks i s bs) ([0 .. ] `zip` ss)

-- |Used by createWorld
createBlocks :: Int -> [String] -> [Block] -> Maybe [(Block,Int)] 
createBlocks i ss bs = mapM (\s -> liftM (\b -> (b,i)) $ getBlock s bs) ss    

-- |Used by createWorld
createBlocks' :: Int -> [String] -> [Block] -> Maybe (Int,[Block]) 
createBlocks' i ss bs = liftM (\bs -> (i,reverse bs)) $ mapM (\s -> getBlock s bs) ss    

----------------------------------------------------------------------- 

-- |Input : a name of a block , any list of blocks
--  Output: The block with the given name from the list if it exist
getBlock :: String -> [Block] -> Maybe Block 
getBlock ss bs = find (\b -> name b == ss) bs

-- |Input: Any block, a word
--  return x-coordinate of the given block if it exist in the world
getBlockIndex :: Block -> World -> Maybe Int
getBlockIndex b w = M.lookup b (indexes w)  

-- |Input:  A x-coordinate, a world 
--  Output: All blocks at the given index if the index exists in the world
getBlocksAt :: Int -> World -> Maybe [Block]
getBlocksAt  i w = M.lookup i $ ground w  

-- |Input: a function, a world
--  Output Returns all blocks an the ground that satifies the given function 
getBlocksOnGroundBy :: (Block -> Bool) -> World -> [Block]    
getBlocksOnGroundBy f w = filter f $ M.keys (indexes w)  

-- |Input:  Any list a blocks, a world 
--  Output: The first occurance of a block with the biggest x-coordinate 
--        if all given blocks exist on the ground in the given list
getRightMost :: [Block] -> World -> Maybe Block 
getRightMost bs w | isNothing mIndexes = Nothing 
                  | otherwise = return . snd $ 
                        maximumBy (\p1 p2 -> compare (fst p1) (fst p2)) (fromJust mIndexes `zip` bs) 
        where mIndexes = mapM  (\b -> getBlockIndex b w) bs  

-- |The same as rightmost but with smallest index
getLeftMost :: [Block] -> World -> Maybe Block 
getLeftMost bs w | isNothing mIndexes = Nothing 
                 | otherwise = return . snd $ 
                        minimumBy (\p1 p2 -> compare (fst p1) (fst p2)) (fromJust mIndexes `zip` bs) 
        where mIndexes = mapM  (\b -> getBlockIndex b w) bs  

-- |Input: any list of blocks, a world
--  Output: If the input blocks havn't the same index nothing is returned else the bottom block from the 
--        input list in the stack  
getUnderMost :: [Block] -> World -> Maybe Block 
getUnderMost bs w = getUpperUnderMost last bs w  

--  |The same as getUnderMost but looks for topmost block
getUpperMost :: [Block] -> World -> Maybe Block 
getUpperMost bs w = getUpperUnderMost head bs w  

-- |Used by getUnderMost and getUpperMost 
getUpperUnderMost :: ([Block] -> Block) -> [Block] -> World -> Maybe Block 
getUpperUnderMost f bs w | null bs || isNothing mBlocks  = Nothing
                         | otherwise  = if length (intersect (fromJust mBlocks)  bs) == length bs then Just (f bs) 
                                    else Nothing
                        where mBlocks = case getBlockIndex (head bs) w of 
                                                    Just i  -> getBlocksAt i w   
                                                    Nothing -> Nothing 

-- |Returns the minimum stack height in the world 
getMinimumStackHeight :: World -> Int 
getMinimumStackHeight w = fromJust $ getMinimumStackHeightFrom w 0  

-- |Works like getMinimumStackHeight but uses a start index to look from
--  getMinimumStackHeight w == getMinimumStackHeightFrom w 0 
getMinimumStackHeightFrom :: World -> Int -> Maybe Int 
getMinimumStackHeightFrom w i | null xs = Nothing  
                              | otherwise = Just $ length . snd $ minimumBy 
                                (\p1 p2 -> compare (length $ snd p1) (length $ snd p2)) $ xs 
                    where xs = drop i (M.toList (ground w))  

getMinimumStackHeightUntil :: World -> Int -> Maybe Int 
getMinimumStackHeightUntil w i | null xs = Nothing  
                              | otherwise = Just $ length . snd $ minimumBy 
                                (\p1 p2 -> compare (length $ snd p1) (length $ snd p2)) $ xs 
                    where xs = take i (M.toList (ground w))  
-- |Works like getMinimumStackHeight but uses a last index to look at
--  getMinimumStackHeight w == getMinimumStackHeightUntil w (worldsize - 1) 

-------------------------------------------------------------------------------------------------------

-- |Returns true if picking the topmost block from stack with index 'i' is a valid instruction in the world
validPickId :: Int -> World -> Bool
validPickId i w 
              | isJust $ holding w = False
              | otherwise = case M.lookup i (ground w) of 
                    Nothing     -> False
                    Just []     -> False  
                    Just (x:xs) -> True

-- |Returns true if block b is the topmost object in its stack and no block is already in holding 
validPick :: Block -> World -> Bool 
validPick b w | isJust $ holding w = False
              | otherwise = 
                case M.lookup b (indexes w) of 
                 Nothing -> False  
                 Just i  -> case M.lookup i (ground w) of 
                                    Nothing     -> False 
                                    Just (x:xs) -> x == b 
                                    Just []     -> False  

-- |Returns true if the world physics allows the block in holding to be put on top of the stack at index i
validDrop :: Int -> World -> Bool
validDrop i w = case holding w  of 
                    Nothing -> False 
                    Just holdBlock  -> case M.lookup i (ground w) of 
                                         Just []     -> True
                                         Just (groundBlock:xs) -> not $ groundBlock < holdBlock
                                         Nothing     -> False   

-- |Returns a Just updated world where the block at stack index i has been picked up, or Nothing if the pick is not legal
pickId :: Int -> World -> Maybe World
pickId i w = case M.lookup i (ground w) of
                Nothing -> Nothing
                Just [] -> Nothing
                Just (b:bs) -> pick b w

-- |Returns a Just updated world where the block b is removed from its stack if it is a valid move.
pick :: Block -> World -> Maybe World 
pick b w | validPick b w = return $ w {holding = Just b, ground = ground $ newWorld, indexes = indexes newWorld} 
         | otherwise     = Nothing 
            where newWorld = deleteBlock b w  

-- |Returns a Just updated world where the block b is added to the stack index i if it is a valid move
dropBlock :: Int -> World -> Maybe World 
dropBlock i w | validDrop i w = return $ w {holding = Nothing, ground = ground $ newWorld, indexes = indexes newWorld}
         | otherwise = Nothing 
            where newWorld = addBlock i (fromJust $ holding w) w

-- |Returns a new world where only the deleted block has been removed but the holding state is left unchanged.
deleteBlock :: Block -> World -> World 
deleteBlock b w = w {ground = M.update (return . tail) i (ground w), indexes = M.delete b (indexes w)}  
        where i = fromJust $ M.lookup b (indexes w)

-- |Returns a new world where only the block b has been added to stack index i without updating the holding state.
addBlock :: Int -> Block -> World -> World 
addBlock i b w = w {indexes = M.insert b i (indexes w), ground = M.update (return . (b :)) i (ground w)} 
