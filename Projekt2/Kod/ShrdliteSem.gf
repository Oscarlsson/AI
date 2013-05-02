
concrete ShrdliteSem of Shrdlite = {

lincat 

S, Thing, Block, Location, Size, Color, Form = Str;

oper

p : Str -> Str;
px : Str -> Str -> Str;
pxy : Str -> Str -> Str -> Str;
pxyz : Str -> Str -> Str -> Str -> Str;

p pred = pred;
px pred x = "(" ++ pred ++ x ++ ")";
pxy pred x y = "(" ++ pred ++ x ++ y ++ ")";
pxyz pred x y z = "(" ++ pred ++ x ++ y ++ z ++ ")";

lin

take t   = px "take" t;
put    l = px "put" l;
move t l = pxy "move" t l;

beside  t = px "beside" t;
leftof  t = px "leftof" t;
rightof t = px "rightof" t;
above   t = px "above" t;
ontop   t = px "ontop" t;
under   t = px "under" t;
inside  t = px "inside" t;

thatis b l = pxy "thatis" b l;

the b = px "the" b;
any b = px "any" b;
all b = px "all" b;

floor = p "floor";

block f s c = pxyz "block" f s c;

anysize  = p "_";
small    = p "small";
medium   = p "medium";
large    = p "large";
wide     = p "wide";
tall     = p "tall";

anycolor = p "_";
black    = p "black";
white    = p "white";
blue     = p "blue";
green    = p "green";
yellow   = p "yellow";
red      = p "red";

anyblock = p "_";
box      = p "box";
pyramid  = p "pyramid";
rectangle= p "rectangle";
square   = p "square";
ball     = p "ball";

}
