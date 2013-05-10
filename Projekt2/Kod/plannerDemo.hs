import PGF
import Data.Maybe
import Shrdlite

type Block = String
type Holding = Maybe Block
type Location = Int

type World = [[[Char]]]
type State = (Holding, World)

type Goal = State -- Kan behöva bestå av GF-haskell-typer

-- 
data Instruction = Drop Location | Pick Location

initialState :: State
initialState = (Nothing,
	[[], ["a","b"], ["c","d"], [], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]]
	)

allLegalMoves :: State -> [Instruction]
allLegalMoves = undefined

finished :: State -> Goal -> Bool
finished = undefined

action :: State -> Instruction -> Maybe State
action s@(_, world) i@(Pick location)
	| isPossible s i = Just (Just pickedBlockId, s')
	| otherwise = Nothing
	where	
			pickedBlockId = last $ world!!location
			s' = map (\l -> filter (\b -> b /= pickedBlockId) l) world
action s@(Just holding, world) i@(Drop location) 
	| isPossible s i = Just s'
	| otherwise = Nothing 
	where 
			ix = [0..length world - 1]
			worldWithoutPickedItem = map (\i -> if (i == location) then ((world!!i) ++ [holding]) else (world!!i)) ix
			s' = (Nothing, worldWithoutPickedItem) 

--- Methods for legal moves
legalDrop :: GBlock -> GBlock -> Bool
legalDrop (Gblock _ _ _) (Gblock _ _ _) = True

isPossible :: State -> Instruction -> Bool
isPossible (Just _, _) (Pick _) = False
isPossible (Nothing, _) (Drop _) = False
isPossible (_, world) (Pick location) = not $ null $ world!!location 

isPossible s@(_, world) (Drop location) 
	| isNothing stack = True
	| otherwise = legalDrop (fromJust $ blockAtHolding s) (fromJust $ stack)
	where stack = blockAtLocation s location





--- Methods to retrieve GBlock
blockAtHolding :: State -> Maybe GBlock
blockAtHolding s@(Just a, world) = Just $ tempBlock $ a
blockAtHolding s@(Nothing, world) = Nothing 

blockAtLocation :: State -> Location -> Maybe GBlock
blockAtLocation (holding, world) location 
	| null stack = Nothing
	| otherwise = Just $ tempBlock $ last stack 
	where stack = world!!location 

tempBlock :: String -> GBlock
tempBlock "a" = Gblock Grectangle Gtall Gblue
tempBlock "b" = Gblock Gball Gtall Gblue
tempBlock "c" = Gblock Gsquare Gtall Gblue
tempBlock "d" = Gblock Gpyramid Gtall Gblue
tempBlock "e" = Gblock Gbox Gtall Gblue
tempBlock "f" = Gblock Grectangle Gtall Gblue
tempBlock "g" = Gblock Grectangle Gtall Gblue
tempBlock "h" = Gblock Grectangle Gtall Gblue
tempBlock "i" = Gblock Gpyramid Gtall Gblue
tempBlock "j" = Gblock Gbox Gtall Gblue
tempBlock "k" = Gblock Gball Gtall Gblue
tempBlock "l" = Gblock Gbox Gtall Gblue
tempBlock "m" = Gblock Gball Gtall Gblue

