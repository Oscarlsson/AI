module Blocks where 

import PGF
import Shrdlite

data Block = B {name :: String ,form :: GForm , size :: GSize , color :: GColor , width :: Double, height :: Double}
    deriving (Show)


--Don't use "compare" when comparing sizes of blocks since they will always be different
--if they don't have equal name  

instance Eq Block where 
    b1 == b2 = name b1 == name b2 
    b1 /= b2 = name b1 /= name b2


instance Ord Block where 
    b1 < b2 = case form b1 of 
               Gball          -> True
               Gpyramid       -> True 
               _              -> width b1 < width b2
  
    b1 > b2 = case form b1 of 
               Gpyramid -> False 
               Gball    -> False 
               _        -> width b1 > width b2

    b1 >= b2 =  
               case form b1 of 
               Gpyramid -> False 
               Gball    -> False 
               _        -> width b1 >= width b2
    b1 <= b2 =  
               case (form b1,form b2) of 
               (Gpyramid,Gpyramid) -> False 
               (Gpyramid,Gpyramid) -> False  
               (Gpyramid,_)        -> True 
               (Gball,_)           -> True 
               _        -> width b1 <= width b2
    b1 `min` b2 = case form b1 of 
               Gpyramid -> b1 
               Gball    -> b1 
               _        -> if width b1 < width b2 then b1 else b2   
    b1 `max` b2 = case form b1 of 
               Gpyramid -> b2 
               Gball    -> b2 
               _        -> if width b1 > width b2 then b1 else b2   
    compare b1 b2 = case (form b1,form b2) of 
               (Gpyramid,Gpyramid) -> compare (name b1) (name b2) 
               (Gball,Gball)       -> compare (name b1) (name b2)  
               (Gpyramid,Gball)    -> compare (name b1) (name b2)  
               (Gball,Gpyramid)    -> compare (name b1) (name b2)  
               _        -> if res == EQ then compare (name b1) (name b2) else res 
                    where res = compare (width b1) (width b2) 


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

