
-- How to use: 
--   compile this file into planner.cgi:
--   ghc -o planner.cgi --make planner.hs

import Network.CGI
import Data.Maybe (fromMaybe, fromJust)
import Data.List (findIndex)
import Control.Monad (liftM)

type Block = String
type World = [[Block]]
type Tree = String


cgiMain :: CGI CGIResult
cgiMain = do setHeader "Content-type" "text/plain"
             (holding, world, trees) <- cgiInput
             let plan = findPlan holding world trees
             output (unlines plan)


findPlan :: Block -> World -> [Tree] -> [String]
findPlan holding world trees 
    = ["# Stupid Haskell planner!",
       "# Holding: " ++ holding,
       "# World: " ++ show world] ++ 
      ["# Tree " ++ show n ++ ": " ++ t | 
       (n, t) <- zip [0..] trees] ++ 
      ["This is a stupid move!",
       "pick " ++ show stacknr,
       "drop " ++ show stacknr]
    where stacknr = fromMaybe 0 (findIndex (not . null) world)


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
