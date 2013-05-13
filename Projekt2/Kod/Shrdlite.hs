module Shrdlite where

import PGF hiding (Tree)
import qualified PGF
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> PGF.Tree
  fg :: PGF.Tree -> a

newtype GString = GString String  deriving Show

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int  deriving Show

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double  deriving Show

instance Gf GFloat where
  gf (GFloat x) = mkDouble x
  fg t =
    case unDouble t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data GBlock =
   Gblock GForm GSize GColor 
 | Gthatis GBlock GLocation 
  deriving (Show,Eq)

data GColor =
   Ganycolor 
 | Gblack 
 | Gblue 
 | Ggreen 
 | Gred 
 | Gwhite 
 | Gyellow 
  deriving (Show,Eq)

data GForm =
   Ganyblock 
 | Gball 
 | Gbox 
 | Gpyramid 
 | Grectangle 
 | Gsquare 
  deriving (Show,Eq)

data GLocation =
   Gabove GThing 
 | Gbeside GThing 
 | Ginside GThing 
 | Gleftof GThing 
 | Gontop GThing 
 | Grightof GThing 
 | Gunder GThing 
  deriving (Show,Eq)

data GS =
   Gmove GThing GLocation 
 | Gput GLocation 
 | Gtake GThing 
  deriving (Show,Eq)

data GSize =
   Ganysize 
 | Glarge 
 | Gmedium 
 | Gsmall 
 | Gtall 
 | Gwide 
  deriving (Show,Eq)

data GThing =
   Gall GBlock 
 | Gany GBlock 
 | Gfloor 
 | Gthe GBlock 
  deriving (Show,Eq)


instance Gf GBlock where
  gf (Gblock x1 x2 x3) = mkApp (mkCId "block") [gf x1, gf x2, gf x3]
  gf (Gthatis x1 x2) = mkApp (mkCId "thatis") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "block" -> Gblock (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "thatis" -> Gthatis (fg x1) (fg x2)


      _ -> error ("no Block " ++ show t)

instance Gf GColor where
  gf Ganycolor = mkApp (mkCId "anycolor") []
  gf Gblack = mkApp (mkCId "black") []
  gf Gblue = mkApp (mkCId "blue") []
  gf Ggreen = mkApp (mkCId "green") []
  gf Gred = mkApp (mkCId "red") []
  gf Gwhite = mkApp (mkCId "white") []
  gf Gyellow = mkApp (mkCId "yellow") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "anycolor" -> Ganycolor 
      Just (i,[]) | i == mkCId "black" -> Gblack 
      Just (i,[]) | i == mkCId "blue" -> Gblue 
      Just (i,[]) | i == mkCId "green" -> Ggreen 
      Just (i,[]) | i == mkCId "red" -> Gred 
      Just (i,[]) | i == mkCId "white" -> Gwhite 
      Just (i,[]) | i == mkCId "yellow" -> Gyellow 


      _ -> error ("no Color " ++ show t)

instance Gf GForm where
  gf Ganyblock = mkApp (mkCId "anyblock") []
  gf Gball = mkApp (mkCId "ball") []
  gf Gbox = mkApp (mkCId "box") []
  gf Gpyramid = mkApp (mkCId "pyramid") []
  gf Grectangle = mkApp (mkCId "rectangle") []
  gf Gsquare = mkApp (mkCId "square") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "anyblock" -> Ganyblock 
      Just (i,[]) | i == mkCId "ball" -> Gball 
      Just (i,[]) | i == mkCId "box" -> Gbox 
      Just (i,[]) | i == mkCId "pyramid" -> Gpyramid 
      Just (i,[]) | i == mkCId "rectangle" -> Grectangle 
      Just (i,[]) | i == mkCId "square" -> Gsquare 


      _ -> error ("no Form " ++ show t)

instance Gf GLocation where
  gf (Gabove x1) = mkApp (mkCId "above") [gf x1]
  gf (Gbeside x1) = mkApp (mkCId "beside") [gf x1]
  gf (Ginside x1) = mkApp (mkCId "inside") [gf x1]
  gf (Gleftof x1) = mkApp (mkCId "leftof") [gf x1]
  gf (Gontop x1) = mkApp (mkCId "ontop") [gf x1]
  gf (Grightof x1) = mkApp (mkCId "rightof") [gf x1]
  gf (Gunder x1) = mkApp (mkCId "under") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "above" -> Gabove (fg x1)
      Just (i,[x1]) | i == mkCId "beside" -> Gbeside (fg x1)
      Just (i,[x1]) | i == mkCId "inside" -> Ginside (fg x1)
      Just (i,[x1]) | i == mkCId "leftof" -> Gleftof (fg x1)
      Just (i,[x1]) | i == mkCId "ontop" -> Gontop (fg x1)
      Just (i,[x1]) | i == mkCId "rightof" -> Grightof (fg x1)
      Just (i,[x1]) | i == mkCId "under" -> Gunder (fg x1)


      _ -> error ("no Location " ++ show t)

instance Gf GS where
  gf (Gmove x1 x2) = mkApp (mkCId "move") [gf x1, gf x2]
  gf (Gput x1) = mkApp (mkCId "put") [gf x1]
  gf (Gtake x1) = mkApp (mkCId "take") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "move" -> Gmove (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "put" -> Gput (fg x1)
      Just (i,[x1]) | i == mkCId "take" -> Gtake (fg x1)


      _ -> error ("no S " ++ show t)

instance Gf GSize where
  gf Ganysize = mkApp (mkCId "anysize") []
  gf Glarge = mkApp (mkCId "large") []
  gf Gmedium = mkApp (mkCId "medium") []
  gf Gsmall = mkApp (mkCId "small") []
  gf Gtall = mkApp (mkCId "tall") []
  gf Gwide = mkApp (mkCId "wide") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "anysize" -> Ganysize 
      Just (i,[]) | i == mkCId "large" -> Glarge 
      Just (i,[]) | i == mkCId "medium" -> Gmedium 
      Just (i,[]) | i == mkCId "small" -> Gsmall 
      Just (i,[]) | i == mkCId "tall" -> Gtall 
      Just (i,[]) | i == mkCId "wide" -> Gwide 


      _ -> error ("no Size " ++ show t)

instance Gf GThing where
  gf (Gall x1) = mkApp (mkCId "all") [gf x1]
  gf (Gany x1) = mkApp (mkCId "any") [gf x1]
  gf Gfloor = mkApp (mkCId "floor") []
  gf (Gthe x1) = mkApp (mkCId "the") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "all" -> Gall (fg x1)
      Just (i,[x1]) | i == mkCId "any" -> Gany (fg x1)
      Just (i,[]) | i == mkCId "floor" -> Gfloor 
      Just (i,[x1]) | i == mkCId "the" -> Gthe (fg x1)


      _ -> error ("no Thing " ++ show t)


