
abstract Shrdlite = {

cat 

S;
Thing;
Block;
Location;
Size;
Color;
Form;

fun

take : Thing -> S;
put  : Location -> S;
move : Thing -> Location -> S;

beside,
leftof,
rightof,
above,
ontop,
under,
inside : Thing -> Location;

thatis : Block -> Location -> Block;

the, 
any, 
all : Block -> Thing;

floor : Thing;

block : Form -> Size -> Color -> Block;

anysize, small, medium, large, wide, tall : Size;
anycolor, black, white, blue, green, yellow, red : Color;
anyblock, box, pyramid, rectangle, square, ball : Form;

}
