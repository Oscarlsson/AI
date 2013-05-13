module Blocks where 

import PGF
import Shrdlite

data Block = B {name :: String ,form :: GForm , size :: GSize , color :: GColor , width :: Double, height :: Double}
    deriving (Show,Eq)

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

