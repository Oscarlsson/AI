main :: IO ()

main = do
	print $ 1+1	
	return ()

"""
	Data types
"""
type Block = Char
type Holding = Maybe Block
type Location = Integer

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

action :: State -> Instruction -> State
action s (Drop location) = s -- Todo
action s (Pick location) = s -- Todo

isPossible :: State -> Instruction -> Bool
isPossible (Nothing, world) (Pick location) = null $ world!!location 
isPossible (Just _, _) _ = False
isPossible s (Drop location) = isPointy $ blockAtLocation $ last $ s!!location 

blockAtLocation :: Location -> GBlock
blockAtLocation = undefined


