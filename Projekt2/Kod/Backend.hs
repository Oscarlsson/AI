module Backend where 

import PGF
import Blocks
import qualified Data.Set as S 
import qualified Data.Map as M 
import Control.Monad.State
import Data.Maybe 
import Data.List
import Data.Tuple (swap) 
import Prelude hiding (drop)

type Ground  = M.Map Int [Block]   
type Indexes = M.Map Block Int 

data World = W {holding :: Maybe Block , ground :: Ground, indexes :: Indexes, wsize :: Int}
    deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

findBlockByName :: String -> World -> Maybe Block 
findBlockByName str w | isJust maybeBlock = maybeBlock 
                      | otherwise = if liftM name (holding w) == Just str then holding w else Nothing    
                where maybeBlock = liftM fst . find (\(b,_) -> name b == str) $ M.toList (indexes w)  

--------------------------------------------------------------------------------

isLeftOf :: Block -> Block -> World -> Bool 
isLeftOf b1 b2 w = isOnGround b1 w && isOnGround b2 w 
                && fromJust (liftM2 (>)  (M.lookup b1 (indexes w)) (M.lookup b2 (indexes w)))  

isRightOf :: Block -> Block -> World -> Bool 
isRightOf b1 b2 w = isOnGround b1 w && isOnGround b2 w 
                && fromJust (liftM2 (<)  (M.lookup b1 (indexes w)) (M.lookup b2 (indexes w)))    

isAbove :: Block -> Block -> World -> Bool 
isAbove b1 b2 w = isOnGround b1 w && isOnGround b2 w && b2 `elem` xs && 
                fromJust (liftM2 (<)  (findIndex (==b1) xs) (findIndex (==b2) xs))   
            where xs = fromJust $ M.lookup  (fromJust $ M.lookup b1 (indexes w)) (ground w) 

isUnder :: Block -> Block -> World -> Bool 
isUnder b1 b2 w = isOnGround b1 w && isOnGround b2 w && b2 `elem` xs && 
                fromJust (liftM2 (>)  (findIndex (==b1) xs) (findIndex (==b2) xs))   
            where xs = fromJust $ M.lookup  (fromJust $ M.lookup b1 (indexes w)) (ground w) 

isOnGround :: Block -> World -> Bool 
isOnGround b w = M.member b $ indexes w   

isOnTop :: Block -> Block -> World -> Bool 
isOnTop b1 b2 w = liftM2 (==) mi1 mi2 == Just True && (head . fromJust $ M.lookup (fromJust mi1) (ground w)) == b2 
            where (mi1, mi2) = (getBlockIndex b1 w, getBlockIndex b2 w) 
    
isOnBottom :: Block -> World -> Bool 
isOnBottom b w | isJust mi = (last . fromJust $ M.lookup (fromJust mi) (ground w)) == b
               | otherwise = False  
            where mi = getBlockIndex b w 
    

isBeside :: Block -> Block -> World -> Bool 
isBeside b1 b2 w = isLeftOf b1 b2 w || isRightOf b1 b2 w
   
isHolding :: Block -> World -> Bool
isHolding b w = liftM (== name b) (liftM name (holding w)) == Just True  

isEmptyIndex :: Int -> World -> Bool 
isEmptyIndex i w = case M.lookup i (ground w) of 
                        Just [] -> True
                        _       -> False  

--------------------------------------------------------------------------------

createWorld :: [[String]] -> String -> [Block] -> Maybe World
createWorld ss "" bs = case (createGround ss bs , createIndexes ss bs) of 
                            (Just gr,Just ind) -> Just $ W {ground = gr , indexes = ind, 
                                                            holding = Nothing, wsize = length ss}
                            _                  -> Nothing 
createWorld ss hol bs = case (createGround ss bs , createIndexes ss bs,getBlock hol bs) of 
                            (Just gr,Just ind, Just hol') -> 
                                Just $ W {ground = gr , indexes = ind, holding = Just hol', wsize = length ss}
                            _                             -> Nothing 

createGround :: [[String]] -> [Block] -> Maybe Ground
createGround ss bs = liftM M.fromList . sequence $ map (\(i,s) ->createBlocks' i s bs) ([0 .. ] `zip` ss)

createIndexes :: [[String]] -> [Block] -> Maybe Indexes 
createIndexes ss bs = liftM (M.fromList . concat) . sequence $ map (\(i,s) ->createBlocks i s bs) ([0 .. ] `zip` ss)

createBlocks :: Int -> [String] -> [Block] -> Maybe [(Block,Int)] 
createBlocks i ss bs = mapM (\s -> liftM (\b -> (b,i)) $ getBlock s bs) ss    

createBlocks' :: Int -> [String] -> [Block] -> Maybe (Int,[Block]) 
createBlocks' i ss bs = liftM (\bs -> (i,reverse bs)) $ mapM (\s -> getBlock s bs) ss    

getBlock :: String -> [Block] -> Maybe Block 
getBlock ss bs = find (\b -> name b == ss) bs

----------------------------------------------------------------------- 

getBlockIndex :: Block -> World -> Maybe Int
getBlockIndex b w = M.lookup b (indexes w)  

getBlocksOnGroundBy :: (Block -> Bool) -> World -> [Block]    
getBlocksOnGroundBy f w = filter f $ M.keys (indexes w)  

getRightMost :: [Block] -> World -> Maybe Block 
getRightMost bs w | isNothing mIndexes = Nothing 
                  | otherwise = return . snd $ 
                        maximumBy (\p1 p2 -> compare (fst p1) (fst p2)) (fromJust mIndexes `zip` bs) 
        where mIndexes = mapM  (\b -> getBlockIndex b w) bs  

getLeftMost :: [Block] -> World -> Maybe Block 
getLeftMost bs w | isNothing mIndexes = Nothing 
                 | otherwise = return . snd $ 
                        minimumBy (\p1 p2 -> compare (fst p1) (fst p2)) (fromJust mIndexes `zip` bs) 
        where mIndexes = mapM  (\b -> getBlockIndex b w) bs  

--for testing purposes 
initWorld = [[], ["a"], ["c","d"], [], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]]
