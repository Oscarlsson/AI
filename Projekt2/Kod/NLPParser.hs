module NLPParser where

import PGF
import Data.Maybe
import Shrdlite
import Control.Monad 
import Data.List 
import Data.Char

import ErrM 
import Blocks 

data Action = Move | Put | Take | None 
    deriving (Show,Eq) 
data Location =  Above Block | Empty  
                | Beside [Block] | Inside Block | LeftOf Block | OnTop Block | RightOf Block | Under Block  
    deriving (Show,Eq) 
data Output = O {action :: Action , mBlocks :: [Block], location :: Location}
    deriving (Show,Eq)

initOutput :: Output 
initOutput = O {action = None, location = Empty, mBlocks = []}  

command :: String 
command = --"Put the blue block that is to the left of a pyramid in a medium-sized box"
         -- "put all red blocks left of a white box"
        --"Put the blue block that is to the left of a pyramid in a medium-sized box."
        --"Move all blocks inside a box on top of the red square?"
        --"Put the wide blue block under the black rectangle."
        --"move all wide rectangles into a red box"
        --"put all blue blocks in a red box."
        --"take the floor"
        --"take the ball that is left of all blocks"
        -- "take the ball beside the floor"
        --"take the ball that is beside all blocks"
        --"take the tall square"
        --"put a red block beside a blue block"    
        --"take the pyramid that is to the left of all boxes" 
        "put the white ball to the left of all blocks"
        --"move the red box left of all red boxes" #This is possible, we can motivate it
        --"take the red box that is to the left of all boxes"

modifyString :: String -> String 
modifyString xs = filter (\c -> not $ c `elem` ['.',',','!','?',';',':','\'', '\"']) $ map toLower xs

runParser :: PGF -> String -> [Err Output]
runParser shrdPGF com = do  
    --shrdPGF <- readPGF "Shrdlite.pgf"
    let lang = head $ languages shrdPGF
    let exs = parse shrdPGF lang (startCat shrdPGF) $ modifyString com 
    map (traverseTree . fg) exs    

world :: [[String]] 
world = [[], ["a","b"], ["c","d"], [], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]]

type World = [[Block]]

traverseTree :: GS -> Err Output  
traverseTree gs = case gs of 
            (Gmove thing loc) -> let th = handleThing thing in 
                                    case th of
                                        Ok []  -> fail "no such block" 
                                        Ok th' -> liftM (\loc' -> initOutput {action = Move, mBlocks = th',
                                                   location = loc' }) $ getRefLocation loc
                                        Bad s   -> fail s  
            (Gput loc)        -> liftM (\loc' -> initOutput {action = Put, location = loc' })
                                        $ getRefLocation loc 
            (Gtake thing)     -> if thing == Gfloor then fail "can't take the floor" else 
                                 let th = handleThing thing in 
                                    case th of 
                                        Bad s  -> fail s 
                                        Ok []  -> fail "no such block"
                                        Ok th' -> return $ initOutput {action = Take , mBlocks = th'} 

-- TODO think about the differance between "the" and "all"  
handleThing :: GThing -> Err [Block] 
handleThing th = case th of 
        Gfloor  -> undefined 
        Gall b  -> handleGBlock b  
        Gany b  -> liftM (take 1) $ handleGBlock b 
        Gthe b  -> handleGBlock b  

handleLocation :: GLocation -> [Block] -> Err [Block] 
handleLocation loc bs = case loc of 
            Gabove (Gall (Gblock Ganyblock Ganysize Ganycolor))-> fail "can't find block above everything"
            Gabove th@(Gall (Gblock f s c))-> maybe (filterBlocks isAbove th bs) 
                        (\_ -> fail "not possible") (find (\b -> form b == f || size b == s || color b == c) bs) 
            Gabove  th ->  filterBlocks isAbove th bs      
            Gbeside (Gall (Gblock Ganyblock Ganysize Ganycolor))-> fail "can't find block beside everything"
            Gbeside th@(Gall (Gblock f s c))-> maybe (filterBlocks' isBeside th bs) 
                        (\_ -> fail "not possible") (find (\b -> form b == f || size b == s || color b == c) bs) 
            Gbeside Gfloor -> fail "can't find block beside the floor"
            Gbeside th -> filterBlocks' isBeside th bs 
            Ginside (Gall (Gblock Ganyblock Ganysize Ganycolor))-> fail "can't find block inside everything"
            Ginside Gfloor -> fail "can't find block inside the floor"
            Ginside th@(Gall (Gblock f s c))-> maybe (filterBlocks isAbove th bs) 
                        (\_ -> fail "not possible") (find (\b -> form b == f || size b == s || color b == c) bs) 
            Ginside th -> filterBlocks isAbove th bs  
            Gleftof (Gall (Gblock Ganyblock Ganysize Ganycolor))-> fail "can't find block left of everything"
            Gleftof th@(Gall (Gblock f s c))-> maybe (filterBlocks' isLeftOf th bs) 
                        (\_ -> fail "not possible") (find (\b -> form b == f || size b == s || color b == c) bs) 
            Gleftof Gfloor -> fail "can't find block left of the floor"
            Gleftof th -> filterBlocks' isLeftOf th bs  
            Gontop (Gall (Gblock Ganyblock Ganysize Ganycolor))-> fail "can't find block on top of everything"
            Gontop th@(Gall (Gblock f s c))-> maybe (filterBlocks isOnTop th bs) 
                        (\_ -> fail "not possible") (find (\b -> form b == f || size b == s || color b == c) bs) 
            Gontop th  -> filterBlocks isOnTop th bs       
            Grightof (Gall (Gblock Ganyblock Ganysize Ganycolor))-> fail "can't find block right of everything"
            Grightof th@(Gall (Gblock f s c))-> maybe (filterBlocks' isRightOf th bs) 
                        (\_ -> fail "not possible") (find (\b -> form b == f || size b == s || color b == c) bs) 
            Grightof Gfloor -> fail "can't find block right of the floor" 
            Grightof th-> filterBlocks' isRightOf th bs  
            Gunder (Gall (Gblock Ganyblock Ganysize Ganycolor))-> fail "can't find block under everything"
            Gunder th@(Gall (Gblock f s c))-> maybe (filterBlocks isUnder th bs) 
                        (\_ -> fail "not possible") (find (\b -> form b == f || size b == s || color b == c) bs) 
            Gunder Gfloor -> fail "cant't find block under the floor" 
            Gunder th  -> filterBlocks isUnder th bs 

validForm :: GLocation -> [Block] -> Err [Block] 
validForm  = undefined   

filterBlocks :: (((Block,Block),[String]) -> Bool) -> GThing -> [Block] -> Err [Block]
filterBlocks f th bs = let hth = handleThing th in 
                            case hth of 
                                Bad s -> fail s 
                                Ok  a -> return $ map (snd . fst) $ filter f ((pairBlocks a bs) `zip` world) 

filterBlocks' ::  ([[String]] -> (Block,Block) ->  Bool) -> GThing -> [Block] -> Err [Block]
filterBlocks' f th bs = let hth = handleThing th in 
                case hth of 
                    Bad s -> fail s 
                    Ok a  -> return $ map snd $ filter (f world) (pairBlocks a bs) 

pairBlocks :: [Block] -> [Block] -> [(Block,Block)] 
pairBlocks th bs = concatMap (\b -> (iterate id b) `zip` bs) th       

isAbove :: ((Block,Block),[String]) -> Bool 
isAbove ((b1,b2),xs) = name b1 `elem` xs && name b2 `elem` xs && 
                        fromJust (liftM2 (<) (elemIndex (name b1) xs) (elemIndex (name b2) xs))    

isUnder :: ((Block,Block),[String]) -> Bool 
isUnder ((b1,b2),xs) = name b1 `elem` xs && name b2 `elem` xs && 
                        fromJust (liftM2 (>) (elemIndex (name b1) xs) (elemIndex (name b2) xs))    

isOnTop :: ((Block,Block),[String]) -> Bool 
isOnTop ((b1,b2),xs) = name b1 `elem` xs && name b2 `elem` xs && last xs == name b2 

isHolding :: Block -> [[String]] -> Bool
isHolding b w = not $ or $ map (\ss -> name b `elem` ss) w

isLeftOf :: [[String]] -> (Block,Block) -> Bool 
isLeftOf w (b1,b2) = (not $ isHolding b1 w) && (or $ map (\ss -> name b2 `elem` ss) ys)
            where ys = takeWhile (\ss -> not $ name b1 `elem` ss) w 

isRightOf :: [[String]] -> (Block,Block) -> Bool
isRightOf w (b1,b2) = or $ map (\ss -> name b2 `elem` ss) ys  
            where ys = drop 1 $ dropWhile (\ss -> not $ name b1 `elem` ss) w  

isBeside :: [[String]] -> (Block,Block) -> Bool 
isBeside ss p = isLeftOf ss p || isRightOf ss p  

--TODO it can be possbile to put something above all blocks depending on the world 
getRefLocation :: GLocation -> Err Location  
getRefLocation loc = case loc of
            Gabove th ->  case handleThing th of 
                            Ok [x] -> if form x == Gpyramid || form x == Gball then fail "invalid form" 
                                        else return (Above x)  
                            _      -> fail "no such block" -- TODO should maybe not just be singelton list 
            Gbeside Gfloor  -> fail "can't put a block beside the floor"
            Gbeside th -> case handleThing th of 
                            Ok ys@(_:_) -> Ok (Beside ys)  
                            _        -> Bad "no such block"
            Ginside Gfloor  -> fail "can't put a block inside the floor"
            Ginside th -> case handleThing th of
                            Ok [x]   -> if form x /= Gbox then fail "invalid form" else return (Inside x) 
                            _        -> fail "location reference must be one object"  
            Gleftof Gfloor  -> fail "can't put a block left of the floor"
            Gleftof th -> case handleThing th of 
                            Ok ys@(_:_) -> return . LeftOf $ xMost  
                                                    (minimumBy (\a b -> compare (fst a) (fst b))) ys  
                            _      -> Bad "no such block"
            Gontop th ->  case handleThing th of 
                            Ok [x] -> if form x == Gpyramid || form x == Gball then fail "invalid form" 
                                        else return (OnTop x)  
                            _ -> Bad "no such block"
            Grightof Gfloor  -> fail "can't put a block right of the floor"
            Grightof th -> case handleThing th of 
                             Ok ys@(_:_) -> return . RightOf $ xMost 
                                                    (maximumBy (\a b -> compare (fst a) (fst b)))  ys  
                             _  -> Bad "no such block"
            Gunder Gfloor  -> fail "can't put a block under the floor"
            Gunder th -> case handleThing th of 
                            Ok (x:xs) -> Ok (Under x)
                            _ -> Bad "no such block"

handleGBlock :: GBlock -> Err [Block]   
handleGBlock b = case b of
        Gblock f s c -> return $ handleBlock b  
        Gthatis b l  -> handleGBlock b >>= \b' -> handleLocation l b' 

handleBlock :: GBlock -> [Block]  
handleBlock gb = case gb of
    (Gblock Ganyblock Ganysize Ganycolor) -> getBlocks (\_ -> True) blocks    
    (Gblock f Ganysize Ganycolor)  -> getBlocks (\b ->  form b == f) blocks 
    (Gblock Ganyblock s Ganycolor) -> getBlocks (\b ->  size b == s) blocks   
    (Gblock Ganyblock Ganysize c) ->  getBlocks (\b ->  color b == c) blocks   
    (Gblock f s Ganycolor) -> getBlocks (\b ->  size b == s && form b == f) blocks  
    (Gblock f Ganysize c) -> getBlocks (\b ->  form b == f && color b == c) blocks   
    (Gblock Ganyblock s c) -> getBlocks (\b ->  size b == s && color b == c) blocks   
    (Gblock f s c) -> getBlocks (\b ->  size b == s && form b == f && color b == c) blocks    

getBlock :: ([[String]] -> Block -> Bool) -> [Block] -> Block 
getBlock f xs = fromJust $ find (f world) xs 

xMost :: ([(Int,Block)] -> (Int,Block)) -> [Block] -> Block
xMost f xs = snd . f $ map (\b -> (fromJust $ findIndex (\ss -> name b `elem` ss) world, b)) xs      

getBlocks :: (Block -> Bool) -> [Block] -> [Block]
getBlocks f bs = filter (\b -> f b) bs  
