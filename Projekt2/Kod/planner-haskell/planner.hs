
-- How to use: 
--   compile this file into planner.cgi:
--   ghc -o planner.cgi --make planner.hs

import Network.CGI
import Data.Maybe (fromMaybe, fromJust)
import Data.List (findIndex)
import Control.Monad (liftM)
import Planner

type Block = String
type World = [[Block]]
type Tree = String


cgiMain :: CGI CGIResult
cgiMain = do setHeader "Content-type" "text/plain"
             (holding, world, trees) <- cgiInput
             let plan = findPlan holding world trees
             output (unlines plan)


findPlan :: Block -> World -> [Tree] -> [String]
findPlan holding word trees = ["pick 1", "drop 8"]


cgiInput :: CGI (Block, World, [Tree])
cgiInput = do holding <- liftM (fromMaybe "") (getInput "holding")
              worldStr <- liftM (fromMaybe "") (getInput "world")
              let world = [split ',' stack | stack <- split ';' worldStr]
              treesStr <- liftM (fromMaybe "") (getInput "trees")
              let trees = split ';' treesStr
              return (holding, world, trees)


split :: Char -> String -> [String]
split delim str
    | rest == "" = if null token then [] else [token] 
    | otherwise  = token : split delim (tail rest)
    where (token, rest) = span (/=delim) str


main :: IO ()
main = runCGI (handleErrors cgiMain)
