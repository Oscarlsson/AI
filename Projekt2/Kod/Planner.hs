module Planner where

import Backend
import Blocks
import Data.Maybe

type History = [Instruction]
type Node = (World, History)
---type PQ = PSQ Node Int
type Seen = [World]

main :: IO ()
main = do
	print $ allLegalMoves w
	where w = fromJust $ createWorld world "b" blocks





data Instruction = Drop Int | Pick Int deriving (Show, Eq)
instance Ord Instruction where
	Pick l1 `compare` Pick l2 = l1 `compare` l2
	Drop l1 `compare` Drop l2 = l1 `compare` l2
	Pick l1 `compare` Drop l2 = l1 `compare` l2
	Drop l1 `compare` Pick l2 = l1 `compare` l2

--------------------------------------------------------------------------------

allLegalMoves :: World -> [Instruction]
allLegalMoves w
				| isJust $ holding w = filter (\instr -> validInstruction instr w) (map Drop (M.keys (ground w)))
				| otherwise = filter (\instr -> validInstruction instr w) (map Pick (M.keys (ground w)))

--------------------------------------------------------------------------------

validInstruction :: Instruction -> World -> Bool
validInstruction i w = case i of 
				Pick x -> validPickId x w 
				Drop x -> validDrop x w 

validPickId :: Int -> World -> Bool
validPickId i w 
			  | isJust $ holding w = False
			  | otherwise = case M.lookup i (ground w) of 
					Nothing		-> False
					Just []		-> False  
					Just (x:xs) -> True

validPick :: Block -> World -> Bool 
validPick b w | isJust $ holding w = False      
              | otherwise = 
                case M.lookup b (indexes w) of 
                 Nothing -> False  
                 Just i  -> case M.lookup i (ground w) of 
                                    Nothing     -> False 
                                    Just (x:xs) -> x == b 
                                    Just []     -> False  

validDrop :: Int -> World -> Bool
validDrop i w = case holding w  of 
                    Nothing -> False 
                    Just holdBlock  -> case M.lookup i (ground w) of 
                                         Just []     -> True
                                         Just (groundBlock:xs) -> groundBlock <= holdBlock
                                         Nothing     -> False   

