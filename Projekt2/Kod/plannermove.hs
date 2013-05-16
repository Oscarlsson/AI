
-- How to use: 
--   compile this file into planner.cgi:
--   ghc -o www/cgi-bin/planner.cgi --make plannermove.hs

import Network.CGI
import Data.Maybe (fromMaybe, fromJust)
import Data.List (findIndex)
import Control.Monad (liftM)
import Planner
import Backend
import Blocks
import PGF 
import NLPParser as P
--type Block = String
--type World = [[Block]]

cgiMain :: CGI CGIResult
cgiMain = do setHeader "Content-type" "text/plain"
             (holding, world, command) <- cgiInput
             -- gör om trees till input sträng.
             -- Hitta planerade rutten
             shrdPGF <- liftIO $ readPGF "/home/oscar/Dev/TIN171AI/Projekt2/Kod/www/cgi-bin/Shrdlite.pgf"
--             error $ show o 
             let w = getWorld holding world
             let o = createGoal (handleOutput $ head $ P.runParser shrdPGF command w) w
             let plan = findPlan w o
             output (unlines plan)


getWorld :: String -> [[String]] -> World
getWorld holding world = fromJust $ createWorld world holding blocks 


findPlan :: World -> Goal ->[String]
findPlan w o = map show (fromJust $  astar w o)


cgiInput :: CGI (String, [[String]], String)
cgiInput = do holding <- liftM (fromMaybe "") (getInput "holding")
              worldStr <- liftM (fromMaybe "") (getInput "world")
              let world = [split ',' stack | stack <- split ';' worldStr]
              treesStr <- liftM (fromMaybe "") (getInput "trees")
              let trees = treesStr
              return (holding, world, trees)


split :: Char -> String -> [String]
split delim str
    | rest == "" = if null token then [] else [token] 
    | otherwise  = token : split delim (tail rest)
    where (token, rest) = span (/=delim) str


main :: IO ()
main = runCGI (handleErrors cgiMain)