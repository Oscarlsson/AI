--# -path=.:alltenses

abstract ShrdliteLex = Cat ** {

fun

pickup_V2,
put_V2,
move_V2,
drop_V2,
move_V2,
take_V2 : V2;

beside_P,
leftof_P,
rightof_P,
above_P,
ontop_P,
under_P,
inside_P : Prep;

empty_A : A;

small_A,
medium_A,
large_A,
wide_A,
tall_A : A;

black_A,
white_A,
blue_A,
green_A,
yellow_A,
red_A : A;

floor_N,
block_N,
box_N,
pyramid_N,
rectangle_N,
square_N,
ball_N : N;

}
