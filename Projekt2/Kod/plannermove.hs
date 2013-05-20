module Main where 
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
import ErrM 
import System.Directory 

--type Block = String
--type World = [[Block]]

cgiMain :: CGI CGIResult
cgiMain = do setHeader "Content-type" "text/plain"
             (holding, world, command) <- cgiInput
             -- gör om trees till input sträng.
             -- Hitta planerade rutten
             path <- liftIO $ getCurrentDirectory 
             shrdPGF <- liftIO $ readPGF (path ++ "/cgi-bin/Shrdlite.pgf")
             case getWorld holding world of 
                  Just w -> do 
                        let xs = map (liftM (\parse -> createGoal parse w)) $ P.runParser shrdPGF command w
                        runFindPlan xs w 
                  _      -> output "error when parsing world"

runFindPlan :: [Err Goal] -> World -> CGI CGIResult 
runFindPlan []  _     = output "I'm  sorry, I didn't understand that (couldn't parse)"
runFindPlan [Bad s] _ = output s 
runFindPlan (x : xs)  w = case x of 
            Ok o  -> output . unlines $ findPlan w o
            _     -> runFindPlan xs w  

getWorld :: String -> [[String]] -> Maybe World
getWorld holding world = createWorld world holding blocks 

findPlan :: World -> Goal ->[String]
findPlan w o = case astar w o of 
                    Just xs -> map show xs 
                    _       -> ["error in astar algorithm"]

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
