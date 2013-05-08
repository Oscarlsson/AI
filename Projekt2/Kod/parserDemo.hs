import PGF
import Data.Maybe
import Shrdlite
main :: IO ()

main = do  
	shrdPGF <- readPGF "Shrdlite.pgf"
	let lang = head $ languages shrdPGF
 	let ex = head $ parse shrdPGF lang (startCat shrdPGF) "move the yellow pyramid into the red box"
	print ( fg ex :: GS )
	return ()
