--# -path=.:alltenses

concrete ShrdliteLexEng of ShrdliteLex = CatEng ** open ParadigmsEng, (I=IrregEng) in {

lin

pickup_V2 = mkV2 (partV (mkV "pick") "up");
drop_V2 = mkV2 "drop";
move_V2 = mkV2 "move";
take_V2 = mkV2 I.take_V | mkV2 "grasp" | pickup_V2;
put_V2  = mkV2 I.put_V | drop_V2;

beside_P  = mkPrep "beside";
leftof_P  = mkPrep "to the left of" | mkPrep "left of";
rightof_P = mkPrep "to the right of" | mkPrep "right of";
above_P   = mkPrep "above";
ontop_P   = mkPrep "on top of" | mkPrep "on";
under_P   = mkPrep "under";
inside_P  = mkPrep "inside" | mkPrep "in" | mkPrep "into";

-- this is cheating with the resource grammar!
empty_A  = mkA "" "" "" "";

small_A  = mkA "small";
medium_A = mkA "medium" | mkA "medium-sized";
large_A  = mkA "large" | mkA "big";
wide_A   = mkA "wide";
tall_A   = mkA "tall";

black_A  = mkA "black";
white_A  = mkA "white";
blue_A   = mkA "blue";
green_A  = mkA "green";
yellow_A = mkA "yellow";
red_A    = mkA "red";

floor_N     = mkN "floor";
block_N     = mkN "block";
box_N       = mkN "box";
pyramid_N   = mkN "pyramid";
rectangle_N = mkN "rectangle";
square_N    = mkN "square";
ball_N      = mkN "ball";

}
