module Test where

import PGF
import Data.Maybe (fromJust)
import Shrdlite

import qualified NLPParser as P
import Planner

import Blocks
import ErrM 

import Backend

import Data.PSQueue as PSQ hiding (null, foldl, foldr)

initWorld2 = [[], ["a", "b"], ["c", "d"], [], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]]
--initWorld2 = [[], ["a"], ["c","b"], ["d"], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]]
--
--
initialWorld :: World
initialWorld = fromJust $ createWorld initWorld2 "" blocks

-- Finished test
--initWorld3 = [[], ["a", "b"], ["d"], ["c","j","f","g","h"], ["e"], ["i"], [], ["k"], [], ["l","m"]]
initWorld3 = [[], ["a", "b"], ["d"], ["c","f","g","h"], ["e"], ["i"], [], ["j","k"], [], ["l","m"]]
initialWorldFinished :: World
initialWorldFinished = fromJust $ createWorld initWorld3 "" blocks

showHistory :: History -> String
showHistory [] = ""
showHistory (x:xs) = show x ++ ";" ++ showHistory xs

testHeuristic :: String -> World -> IO ()
testHeuristic stmt w = do
    shrdPGF <- readPGF "Shrdlite.pgf" 
    let o = firstOk $ P.runParser shrdPGF stmt initialWorld
    let g = createGoal o initialWorld
    putStrLn $ "Initial heuristic: " ++ (show $ heuristic w g)
    mapM_ (print) $ map (\a -> (a, heuristic (fromJust $ action a w) g)) (allLegalMoves w)

testStatement :: String -> IO ()
testStatement stmt = do
    shrdPGF <- readPGF "Shrdlite.pgf" 
    let o = firstOk $ P.runParser shrdPGF stmt initialWorld
    let g = createGoal o initialWorld
    let a = astarDebug initialWorld g
    putStrLn $ "\t" ++ stmt
    putStrLn $ "\t\t" ++ ( show $ "Initial heuristic: " ++ (show $ heuristic initialWorld g) )
    putStrLn $ "\t\t" ++ ( show $ "Nodes visited: " ++ (show $ snd a) )
    putStrLn $ "\t\t" ++ ( show $ (showHistory (fromErr $ fst a)) )
    
fromErr :: Err a -> a 
fromErr (Ok a)  = a 
fromErr (Bad s) = error s 

runTests :: IO ()
runTests = do
    putStrLn "*** Some base cases, not part of real project test"
    testStatement "take the red square"
    testStatement "take the green pyramid"
    testStatement "put the black wide block on top of the red square"
    testStatement "put the blue wide rectangle to the left of the red square"
    testStatement "Move all wide rectangles above the red large box"
    testStatement "Move all wide rectangles inside the red large box"
    putStrLn "*** Real test cases"
    testStatement "Put the blue block that is to the left of a pyramid in a medium-sized box."
    testStatement "Move all wide blocks inside a box on top of the red square."
    testStatement "Put the wide blue block under the black rectangle."
    testStatement "Move all wide rectangles into a red box."

-- Delete this
testTest :: String -> IO ()
testTest stmt = do
    shrdPGF <- readPGF "Shrdlite.pgf" 
    let o = firstOk $ P.runParser shrdPGF stmt initialWorld
    let g = createGoal o initialWorld
    print stmt
    putStrLn $ "\t" ++ ( show $ "Initial heuristic: " ++ (show $ heuristic initialWorld g) )
    let w2 = fromJust $ action (Pick 1) initialWorld
    let w3 = fromJust $ action (Drop 8) w2
    let w4 = fromJust $ action (Pick 1) w3
    let w5 = fromJust $ action (Drop 9) w4
    --let w6 = fromJust $ action (Pick 1) w5
    --let w7 = fromJust $ action (Drop 9) w6
    putStrLn $ "\t" ++ ( show $ "Heuristics w2 " ++ (show $ heuristic w2 g) )
    putStrLn $ "\t" ++ ( show $ "Heuristics w3 " ++ (show $ heuristic w3 g) )
    putStrLn $ "\t" ++ ( show $ "Heuristics w4 " ++ (show $ heuristic w4 g) )
    putStrLn $ "\t" ++ ( show $ "Heuristics w5 " ++ (show $ heuristic w5 g) )
    -- putStrLn $ "\t" ++ ( show $ "Heuristics w6 " ++ (show $ heuristic w6 g) )
    print w4
    print w5
    print $ validInstruction (Drop 9) w4 
    print $ validDrop 9 w4
    putStrLn $ "\t" ++ ( show $ "finished w5 " ++ (show $ finished w5 g) )
    putStrLn $ "\t" ++ ( show $ "Heuristics w5 " ++ (show $ heuristic w5 g) )

-- This method is just for quickly testing the parser.
printObject :: String -> IO ()
printObject stmt = do
    shrdPGF <- readPGF "Shrdlite.pgf" 
    let o = firstOk $ P.runParser shrdPGF stmt initialWorld
    print o

firstOk :: [Err a] -> a
firstOk ([]) = error "No ok"
firstOk ((Ok o):_) = o
firstOk [Bad s] = error s
firstOk ((Bad _):xs) = firstOk xs

testFinished :: IO ()
testFinished = do
    shrdPGF <- readPGF "Shrdlite.pgf" 
    let stmt = "Move all wide blocks inside a box on top of the red square"
    let o = firstOk $ P.runParser shrdPGF stmt initialWorld
    print $ finished initialWorldFinished (createGoal o initialWorld)
    print initialWorldFinished
    print initialWorld

astarDebug :: World -> Goal -> (Err History, Int)
astarDebug w g = 
    case fst result of 
        Bad s -> (Bad s , 0)
        Ok  n -> (Ok (history $ n), snd result)
    where 
            y = astar' aStarTimeout (pq w) [] g
            result = (snd $ y, PSQ.size $ fst y)

