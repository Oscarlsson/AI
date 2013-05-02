--# -path=.:alltenses

concrete ShrdliteEng of Shrdlite = open (E=ExtraEng), SyntaxEng, ShrdliteLexEng in {

lincat 

S = Utt;
Thing = NP;
Block = CN;
Location = Adv;
Size = A;
Color = A;
Form = N;

oper 

command : VP -> Utt;
command vp = lin Utt {s = variants{"" | "will you" | "can you" | "could you"} ++
                        variants{"" | "please"} ++ 
                        (mkUtt (mkImp vp)).s ++ 
                        variants{"" | "please" | ", please"} ++ 
                        variants{"" | "." | "!"}};

lin

take b = command (mkVP take_V2 b);
put  l = command (mkVP (mkVP put_V2 (mkNP it_Pron)) l);
move b l = command (mkVP (mkVP (move_V2 | put_V2) b) l);

beside  b = mkAdv beside_P b;
leftof  b = mkAdv leftof_P b;
rightof b = mkAdv rightof_P b;
above   b = mkAdv above_P b;
ontop   b = mkAdv ontop_P b;
under   b = mkAdv under_P b;
inside  b = mkAdv inside_P b;

thatis f l = mkCN f l | mkCN f (mkRS (mkRCl E.that_RP l));

the f = mkNP the_Det f;
any f = mkNP (a_Det | mkDet E.any_Quant) f;
all f = mkNP all_Predet (mkNP aPl_Det f);

floor = mkNP the_Det (mkCN floor_N);

block f s c = mkCN c (mkCN s f) | mkCN s (mkCN c f);

anysize = empty_A;
small   = small_A;
medium  = medium_A;
large   = large_A;
wide    = wide_A;
tall    = tall_A;

anycolor = empty_A;
black    = black_A;
white    = white_A;
blue     = blue_A;
green    = green_A;
yellow   = yellow_A;
red      = red_A;

anyblock  = block_N;
box       = box_N;
pyramid   = pyramid_N;
rectangle = rectangle_N;
square    = square_N;
ball      = ball_N;

}
