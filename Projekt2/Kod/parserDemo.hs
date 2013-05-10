import PGF
import Data.Maybe
import Shrdlite
import Control.Monad.State 

data Block = B {name :: String ,form :: GForm , size :: GSize , color :: GColor , width :: Double, height :: Double}
    deriving Show
data Action = Move | Put | Take
    deriving Show 
data Location =  Above Block 
                | Beside Block | Inside Block | LeftOf Block | Ontop Block | RightOf Block | Under Block  
    deriving Show 
data Output = O {action :: Action , mblocks :: [Block], location :: Location}
    deriving Show


type OutputS a = State Output a 

initOutput :: Output 
initOutput = undefined 

main :: IO ()
main = do  
	shrdPGF <- readPGF "Shrdlite.pgf"
	let lang = head $ languages shrdPGF
 	let ex = head $ parse shrdPGF lang (startCat shrdPGF) "move the yellow pyramid into the red box"
	print $ evalState (traverseTree ( fg ex :: GS )) initOutput 

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

traverseTree :: GS -> OutputS () 
traverseTree gs = case gs of 
            (Gmove thing loc) -> undefined 
            (Gput loc)        -> undefined 
            (Gtake thing)     -> undefined

handleThing :: GThing -> OutputS () 
handleThing th = case th of 
        Gfloor  -> undefined 
        Gall b  -> undefined 
        Gany b  -> undefined 
        Gthe b  -> undefined  

handleLocation :: GLocation -> OutputS () 
handleLocation loc = case loc of 
            Gabove th -> undefined 
            Gbeside th -> undefined 
            Ginside th -> undefined  
            Gleftof th -> undefined 
            Gontop th -> undefined 
            Grightof th -> undefined 
            Gunder th -> undefined 

handleGBlock :: GBlock -> OutputS () 
handleGBlock b = case b of
        Gblock f s c -> undefined 
        Gthatis b l  -> undefined 

handleBlock :: GBlock -> OutputS ()  
handleBlock b = case b of
    (Gblock Ganyblock Ganysize Ganycolor) -> undefined 
    (Gblock f Ganysize Ganycolor) -> undefined 
    (Gblock Ganyblock s Ganycolor) -> undefined 
    (Gblock Ganyblock Ganysize c) -> undefined 
    (Gblock f s Ganycolor) -> undefined 
    (Gblock f Ganysize c) -> undefined 
    (Gblock Ganyblock s c) -> undefined 
    (Gblock f s c) ->  undefined 
 




 
