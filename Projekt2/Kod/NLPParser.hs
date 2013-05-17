module NLPParser where

import PGF
import Data.Maybe
import Shrdlite
import Control.Monad 
import Data.List 
import Data.Char
import Backend 

import ErrM 
import Blocks 

data Action = Move | Put | Take | None 
    deriving (Show,Eq)

data Location =  
    -- |Argument is the block that one should put blocks above. Mustn't be directly 
    -- above this block.    
                  Above Block 
    -- |Needed for initialization. 
                | Empty  
    -- |Argument is the list of possible blocks that one can choose to put blocks beside of.
                | Beside [Block] 
    -- |Argument is the list of possible blocks that one can choose to put blocks inside. 
                | Inside [Block] 
    -- |Argument is the block that one should put blocks to the left of. 
                | LeftOf Block 
    -- |Argument is the block that one should check the index of and then put blocks on top
    -- of the stack which corresponds to that index.
                | OnTop Block 
    -- |Argument is the block that one should put blocks right of. 
                | RightOf Block 
    -- |Argument is the block that one should put blocks under. Mustn't be directly 
    -- under this block.
                | Under Block 
    -- |Argument is indexes that one can choose to put blocks at.     
                | Floor [Int] 
    deriving (Show,Eq) 

data Output = O {
    -- | An action to take. 
    action :: Action , 
    -- | Blocks to move. 
    mBlocks :: [Block],
    -- | An location to put the blocks at.  
    location :: Location}
    deriving (Show,Eq)

initOutput :: Output 
initOutput = O {action = None, location = Empty, mBlocks = []}  

command :: String 
command = --"Put the blue block that is to the left of a pyramid in a medium-sized box"
         -- "put all red blocks left of a white box"
        --"Put the blue block that is to the left of a pyramid in a medium-sized box."
        --"Move all blocks inside a box on top of the red square?" --TODO not working yet 
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
        --"put the white ball to the left of all blocks"
        --"move the red box left of all red boxes" #This is possible, we can motivate it
        --"take the red box that is to the left of all boxes"
        --"put the red block on the floor"
        --"take the blue block left of all red boxes" --TODO takes two copies of the same block
        --"take the blue block right of all red boxes" --TODO takes a top on top of the right most red box
                                                     -- this can be fixed in handle location in Grightof and 
                                                     -- Gleftof by getting all blocks in "th" and chosse the 
                                                     -- righmost or leftmost block  
        --  "put the red box to the left of the green pyramid" -- Fails with "No such block" .. TODO
        --"Take the block that is above a red block" --make the same mistake as "Move all blocks inside a box on top 
                                                   --of the red square?" takes everything under  
        --"take the black wide rectangle in the white box" 
        "put it on the floor"   
        
modifyString :: String -> String 
modifyString xs = filter (\c -> not $ c `elem` ['.',',','!','?',';',':','\'','[',']','\\','\"']) $ map toLower xs

-- |For testing purposes 
tmpMain :: IO () 
tmpMain = do
    shrdPGF <- readPGF "Shrdlite.pgf"
    case createWorld world "a" blocks of
        Nothing -> putStrLn "can't parse world"
        Just w  -> print $ runParser shrdPGF command w 

runParser :: PGF -> String -> World -> [Err Output]
runParser shrdPGF com w = do  
    let lang = head $ languages shrdPGF
    let exs = parse shrdPGF lang (startCat shrdPGF) $ modifyString com
    map (\gs -> traverseTree (fg gs) w) exs   
    --map (\gs -> traverseTree (fg gs) w) exs -- this is correct the above line is for debugging    

-- |there can be copies of the same block in the final mBlocks, this is expected and solved by nub
traverseTree :: GS -> World -> Err Output  
traverseTree gs w = case gs of 
            (Gmove thing loc) -> let th = handleThing thing w in 
                                    case th of
                                        Ok []  -> fail "no such block" 
                                        Ok th' -> liftM (\loc' -> initOutput {action = Move, mBlocks = nub th',
                                                   location = loc' }) $ getRefLocation loc w
                                        Bad s   -> fail s  
            (Gput loc)        -> if isNothing (holding w) then fail "holding no block" 
                                   else liftM (\loc' -> initOutput {action = Put, location = loc',mBlocks =
                                        [fromJust (holding w)] })
                                        $ getRefLocation loc w 
            (Gtake thing)     -> if thing == Gfloor then fail "can't take the floor" else 
                                 let th = handleThing thing w in 
                                    case th of 
                                        Bad s  -> fail s 
                                        Ok []  -> fail "no such block"
                                        Ok th' -> return $ initOutput {action = Take , mBlocks = nub th'} 

-- |TODO think about the differance between "the" and "all"  
handleThing :: GThing -> World -> Err [Block] 
handleThing th w = case th of 
        Gfloor  -> Bad "floor is not correct handeled"  
        Gall b  -> handleGBlock b w  
        Gany b  -> handleGBlock b w --liftM (take 1) $ handleGBlock b w 
        Gthe b  -> handleGBlock b w 

handleLocation :: GLocation -> [Block] -> World -> Err [Block] 
handleLocation loc bs w = case loc of 
            Gabove (Gall (Gblock Ganyblock Ganysize Ganycolor))-> fail "can't find block above everything"
            Gabove th@(Gall (Gblock f s c))-> maybe (filterBlocks isAbove w th bs) 
                        (\_ -> fail "not possible") (find (\b -> form b == f || size b == s || color b == c) bs) 
            --This is the same as Gontop Gfloor, we can motivate it   
            Gabove  Gfloor -> return $ filter (\b -> isOnBottom b w) bs       
            Gabove  th ->  filterBlocks isAbove w th bs      
            Gbeside (Gall (Gblock Ganyblock Ganysize Ganycolor))-> fail "can't find block beside everything"
            Gbeside th@(Gall (Gblock f s c))-> maybe (filterBlocks isBeside w th bs) 
                        (\_ -> fail "not possible") (find (\b -> form b == f || size b == s || color b == c) bs) 
            Gbeside Gfloor -> fail "can't find block beside the floor"
            Gbeside th -> filterBlocks isBeside w th bs 
            Ginside (Gall (Gblock Ganyblock Ganysize Ganycolor))-> fail "can't find block inside everything"
            Ginside Gfloor -> fail "can't find block inside the floor"
            Ginside th@(Gall (Gblock f s c))-> maybe (filterBlocks isAbove w th bs) 
                        (\_ -> fail "not possible") (find (\b -> form b == f || size b == s || color b == c) bs) 
            Ginside th -> filterBlocks isAbove w th bs --something tricky here works with isUnder!?  
            Gleftof (Gall (Gblock Ganyblock Ganysize Ganycolor))-> fail "can't find block left of everything"
            Gleftof th@(Gall (Gblock f s c))-> maybe (filterBlocks isLeftOf w th bs) 
                        (\_ -> fail "not possible") (find (\b -> form b == f || size b == s || color b == c) bs) 
            Gleftof Gfloor -> fail "can't find block left of the floor"
            Gleftof th -> filterBlocks isLeftOf w th bs  
            Gontop  Gfloor -> return $ filter (\b -> isOnBottom b w) bs       
            Gontop (Gall (Gblock Ganyblock Ganysize Ganycolor))-> fail "can't find block on top of everything"
            Gontop th@(Gall (Gblock f s c))-> maybe (filterBlocks isOnTop w th bs) 
                        (\_ -> fail "not possible") (find (\b -> form b == f || size b == s || color b == c) bs) 
            --This is the same as Gabove Gfloor, we can motivate it   
            Gontop th  -> filterBlocks isOnTop w th bs       
            Grightof (Gall (Gblock Ganyblock Ganysize Ganycolor))-> fail "can't find block right of everything"
            Grightof th@(Gall (Gblock f s c))-> maybe (filterBlocks isRightOf w th bs) 
                        (\_ -> fail "not possible") (find (\b -> form b == f || size b == s || color b == c) bs) 
            Grightof Gfloor -> fail "can't find block right of the floor" 
            Grightof th-> filterBlocks isRightOf w th bs  
            Gunder (Gall (Gblock Ganyblock Ganysize Ganycolor))-> fail "can't find block under everything"
            Gunder th@(Gall (Gblock f s c))-> maybe (filterBlocks isUnder w th bs) 
                        (\_ -> fail "not possible") (find (\b -> form b == f || size b == s || color b == c) bs) 
            Gunder Gfloor -> fail "cant't find block under the floor" 
            Gunder th  -> filterBlocks isUnder w th bs 

filterBlocks :: (Block -> Block -> World -> Bool) -> World -> GThing -> [Block] -> Err [Block]
filterBlocks f w th bs = let hth = handleThing th w in
                            case hth of 
                                Bad s -> fail s 
                                Ok a  -> return . map fst $ filter (\(b1,b2) -> f b1 b2 w) (pairBlocks bs a)        

pairBlocks :: [Block] -> [Block] -> [(Block,Block)] 
pairBlocks bs1 bs2 = concatMap (\b -> (iterate id b) `zip` bs2) bs1       

-- |TODO it can be possbile to put something above all blocks depending on the world 
getRefLocation :: GLocation -> World -> Err Location  
getRefLocation loc w = case loc of
            Gabove Gfloor -> return . Floor $ filter (\i -> isEmptyIndex i w) [0 .. wsize w - 1]       
            Gabove th ->  case handleThing th w of 
                            Ok [x] -> if form x == Gpyramid || form x == Gball then fail "invalid form" 
                                        else return (Above x)  
                            _      -> fail "no such block" -- TODO should maybe not just be singelton list 
            Gbeside Gfloor  -> fail "can't put a block beside the floor"
            Gbeside th -> case handleThing th w of 
                            Ok ys@(_:_) -> Ok (Beside ys)  
                            _           -> Bad "no such block"
            Ginside Gfloor  -> fail "can't put a block inside the floor"
            Ginside th -> case handleThing th w of
                            Ok ys@(_:_) -> if or $ map (\x -> form x /= Gbox) ys then fail "invalid form" 
                                                else return (Inside ys)
                            --Ok (_:_) -> fail "location reference must be one object"   
                            _        -> fail "no such block"  
            Gleftof Gfloor  -> fail "can't put a block lef. of the floor"
            Gleftof th -> case handleThing th w of 
                            Ok ys@(_:_) -> return . LeftOf . fromJust $ getLeftMost ys w  
                            _      -> Bad "no such block"
            Gontop Gfloor ->  return . Floor $ filter (\i -> isEmptyIndex i w) [0 .. wsize w - 1]       
            Gontop th ->  case handleThing th w of 
                            Ok [x] -> if form x == Gpyramid || form x == Gball then fail "invalid form" 
                                        else return (OnTop x)  
                            _ -> Bad "no such block"
            Grightof Gfloor  -> fail "can't put a block right of the floor"
            Grightof th -> case handleThing th w of 
                             Ok ys@(_:_) -> return . RightOf . fromJust $ getRightMost ys w 
                             _  -> Bad "no such block"
            Gunder Gfloor  -> fail "can't put a block under the floor"
            Gunder th -> case handleThing th w of 
                            Ok (x:xs) -> Ok (Under x)
                            _ -> Bad "no such block"

handleGBlock :: GBlock -> World -> Err [Block]   
handleGBlock b w = case b of
        Gblock f s c -> return $ handleBlock b w  
        Gthatis b l  -> handleGBlock b w >>= \b' -> handleLocation l b' w 

handleBlock :: GBlock -> World -> [Block]  
handleBlock gb w = case gb of
    (Gblock Ganyblock Ganysize Ganycolor) -> getBlocksOnGroundBy (\_ -> True) w    
    (Gblock f Ganysize Ganycolor)  -> getBlocksOnGroundBy (\b ->  form b == f) w 
    (Gblock Ganyblock s Ganycolor) -> getBlocksOnGroundBy (\b ->  size b == s) w   
    (Gblock Ganyblock Ganysize c) ->  getBlocksOnGroundBy (\b ->  color b == c) w 
    (Gblock f s Ganycolor) -> getBlocksOnGroundBy (\b ->  size b == s && form b == f) w 
    (Gblock f Ganysize c) -> getBlocksOnGroundBy (\b ->  form b == f && color b == c) w   
    (Gblock Ganyblock s c) -> getBlocksOnGroundBy (\b ->  size b == s && color b == c) w   
    (Gblock f s c) -> getBlocksOnGroundBy (\b ->  size b == s && form b == f && color b == c) w    

-- |For debugging purposes 
world :: [[String]] 
world = [[], ["b"], ["c","d"], [], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]]
 
