module Backend where 

import PGF
import Blocks
import qualified Data.Set as S 
import qualified Data.Map as M 
import Control.Monad.State
import Data.Maybe 
import Data.List
import Data.Tuple (swap) 

type Ground  = M.Map Int [Block]   
type Indexes = M.Map Block Int 

data World = W {holding :: Maybe Block , ground :: Ground, indexes :: Indexes} 
    deriving (Show) 
data Instruction = Put | Get 

--------------------------------------------------------------------------------

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
                    Just b  -> case M.lookup i (ground w) of 
                                    Just []     -> True
                                    Just (x:xs) -> b <= x
                                    Nothing     -> False   

--------------------------------------------------------------------------------

--Also check holding 
findBlockByName :: String -> World -> Maybe Block 
findBlockByName str w = liftM fst . find (\(b,_) -> name b == str) $ M.toList (indexes w)  


--------------------------------------------------------------------------------

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
        where i = fromJust $ M.lookup b (indexes w)

--------------------------------------------------------------------------------

isLeftOf :: Block -> Block -> World -> Bool 
isLeftOf b1 b2 w = isOnGround b1 w && isOnGround b2 w 
                && fromJust (liftM2 (<)  (M.lookup b1 (indexes w)) (M.lookup b2 (indexes w)))  

isRightOf :: Block -> Block -> World -> Bool 
isRightOf b1 b2 w = isOnGround b1 w && isOnGround b2 w 
                && fromJust (liftM2 (>)  (M.lookup b1 (indexes w)) (M.lookup b2 (indexes w)))    

isOnTopOf :: Block -> Block -> World -> Bool 
isOnTopOf b1 b2 w = isOnGround b1 w && isOnGround b2 w && b2 `elem` xs && 
                fromJust (liftM2 (<)  (findIndex (==b1) xs) (findIndex (==b2) xs))   
            where xs = fromJust $ M.lookup  (fromJust $ M.lookup b1 (indexes w)) (ground w) 

isUnder :: Block -> Block -> World -> Bool 
isUnder b1 b2 w = isOnGround b1 w && isOnGround b2 w && b2 `elem` xs && 
                fromJust (liftM2 (>)  (findIndex (==b1) xs) (findIndex (==b2) xs))   
            where xs = fromJust $ M.lookup  (fromJust $ M.lookup b1 (indexes w)) (ground w) 

isOnGround :: Block -> World -> Bool 
isOnGround b w = M.member b $ indexes w   


--------------------------------------------------------------------------------

allLegalMoves :: World -> [Instruction]
allLegalMoves = undefined 

--------------------------------------------------------------------------------

createWorld :: [[String]] -> String -> [Block] -> Maybe World
createWorld ss "" bs = case (createGround ss bs , createIndexes ss bs) of 
                            (Just gr,Just ind) -> Just $ W {ground = gr , indexes = ind, holding = Nothing}
                            _                  -> Nothing 
createWorld ss hol bs = case (createGround ss bs , createIndexes ss bs,getBlock hol bs) of 
                            (Just gr,Just ind, Just hol') -> 
                                Just $ W {ground = gr , indexes = ind, holding = Just hol'}
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
 
--for testing purposes 
world = [[], ["a"], ["c","d"], [], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]]
