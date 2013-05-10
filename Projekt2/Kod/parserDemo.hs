import PGF
import Data.Maybe
import Shrdlite
import Control.Monad.State 
import ErrM 
import Data.List 

data Block = B {name :: String ,form :: GForm , size :: GSize , color :: GColor , width :: Double, height :: Double}
    deriving (Show,Eq)
data Action = Move | Put | Take | None 
    deriving (Show,Eq) 
data Location =  Above Block | Empty  
                | Beside Block | Inside Block | LeftOf Block | OnTop Block | RightOf Block | Under Block  
    deriving (Show,Eq) 
data Output = O {action :: Action , mBlocks :: [Block], location :: Location}
    deriving (Show,Eq)

initOutput :: Output 
initOutput = O {action = None, location = Empty, mBlocks = []}  

command :: String 
command = --"Put the blue block that is to the left of a pyramid in a medium-sized box"
            "move a red box above a white box"
main :: IO ()
main = do  
    shrdPGF <- readPGF "Shrdlite.pgf"
    let lang = head $ languages shrdPGF
    let exs = parse shrdPGF lang (startCat shrdPGF) command
    let ex = if null exs then error "no parse" else head exs
    print (fg ex :: GS)  
    case traverseTree (fg ex) of 
        Ok o -> print o 
        Bad s -> print s 

blocks :: [Block]
blocks = [
    B {name = "a" , form = Grectangle, size = Gtall, color = Gblue, width = 0.50, height = 1.00 },
    B {name = "b" , form = Gball, size = Gsmall , color = Gwhite,  width = 0.50, height = 0.50 },
    B {name = "c" , form = Gsquare,size = Glarge , color = Gred,    width = 1.00, height = 1.00 },
    B {name = "d" , form = Gpyramid , size = Glarge,  color = Ggreen,  width = 1.00, height = 1.00 },
    B {name = "e" , form = Gbox, size = Glarge , color = Gwhite , width = 1.00, height = 0.75 },
    B {name = "f" , form = Grectangle , size = Gwide, color = Gblack, width = 1.00, height = 0.50 },
    B {name = "g" , form = Grectangle, size = Gwide, color = Gblue, width = 1.00, height = 0.50 },
    B {name = "h" , form = Grectangle , size = Gwide, color = Gred,  width = 1.00, height = 0.50 },
    B {name = "i" , form = Gpyramid,  size = Gmedium, color = Gyellow, width = 0.75, height = 0.75 },
    B {name = "j" , form = Gbox,  size = Glarge, color = Gred,  width = 1.00, height = 0.75 },
    B {name = "k" , form = Gball, size = Gsmall, color = Gyellow, width = 0.50, height = 0.50 },
    B {name = "l" , form = Gbox,  size = Gmedium, color = Gred, width = 0.75, height  = 0.50 },
    B {name = "m" , form = Gball, size = Gmedium, color = Gblue, width = 0.75, height = 0.75 }
    ]

world :: [[String]] 
world = [[], ["a","b"], ["c","d"], [], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]]

type World = [[Block]]

traverseTree :: GS -> Err Output  
traverseTree gs = case gs of 
            (Gmove thing loc) -> case getRefLocation loc of 
                                    Ok loc' -> Ok $ initOutput {action = Move, mBlocks = handleThing thing,
                                                location = loc' }
                                    Bad s   -> Bad s   
            (Gput loc)        ->  case getRefLocation loc of 
                                    Ok loc' -> Ok $ initOutput {action = Put, location = loc' }
                                    Bad s   -> Bad s
            (Gtake thing)     -> Ok $ initOutput {action = Take , mBlocks = handleThing thing} 
handleThing :: GThing -> [Block] 
handleThing th = case th of 
        Gfloor  -> undefined 
        Gall b  -> handleGBlock b  
        Gany b  -> take 1 $ handleGBlock b 
        Gthe b  -> handleGBlock b  

handleLocation :: GLocation -> [Block] -> [Block] 
handleLocation loc bs = case loc of 
            Gabove th  -> map (snd . fst) $ filter isAbove ((pairBlocks (handleThing th) bs) `zip` world)     
            Gbeside th -> map snd $ filter (isBeside world) (pairBlocks (handleThing th) bs)  
            Ginside th -> undefined  
            Gleftof th -> map snd $ filter (isLeftOf world) (pairBlocks (handleThing th) bs) 
            Gontop th -> map (snd . fst) $ filter isOnTop ((pairBlocks (handleThing th) bs) `zip` world)      
            Grightof th -> map snd $ filter (isRightOf world) (pairBlocks (handleThing th) bs) 
            Gunder th -> map (snd . fst) $ filter isUnder ((pairBlocks (handleThing th) bs) `zip` world)  


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

isLeftOf :: [[String]] -> (Block,Block) -> Bool 
isLeftOf w (b1,b2) = or $ map (\ss -> name b2 `elem` ss) ys  
            where ys = takeWhile (\ss -> not $ name b1 `elem` ss) w 

isRightOf :: [[String]] -> (Block,Block) -> Bool
isRightOf w (b1,b2) = or $ map (\ss -> name b2 `elem` ss) ys  
            where ys = drop 1 $ dropWhile (\ss -> not $ name b1 `elem` ss) w  

isBeside :: [[String]] -> (Block,Block) -> Bool 
isBeside ss p = isLeftOf ss p || isRightOf ss p  

getRefLocation :: GLocation -> Err Location  
getRefLocation loc = case loc of 
            Gabove th ->  case handleThing th of 
                            []     -> Bad "no such block"
                            (x:xs) -> Ok (Above x)    
            Gbeside th -> case handleThing th of 
                            []     -> Bad "no such block"
                            (x:xs) -> Ok (Beside x)  
            Ginside th -> case handleThing th of 
                            [] -> Bad "no such block"
                            (x:xs) -> Ok (Inside x)  
            Gleftof th -> case handleThing th of 
                            []     -> Bad "no such block"
                            (x:xs) -> Ok (LeftOf x) 
            Gontop th ->  case handleThing th of 
                            [] -> Bad "no such block"
                            (x:xs) -> Ok (OnTop x)  
            Grightof th -> case handleThing th of 
                            [] -> Bad "no such block"
                            (x:xs) -> Ok (RightOf x)  
            Gunder th -> case handleThing th of 
                            [] -> Bad "no such block"
                            (x:xs) -> Ok (Under x)

handleGBlock :: GBlock -> [Block]   
handleGBlock b = case b of
        Gblock f s c -> handleBlock b  
        Gthatis b l  -> handleLocation l (handleGBlock b) 

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

getBlocks :: (Block -> Bool) -> [Block] -> [Block]
getBlocks f bs = filter (\b -> f b) bs  

