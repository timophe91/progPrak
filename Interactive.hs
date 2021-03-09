module Interactive where

import Control.Monad
import Data.Char
import Parser
import Data.Either
import Type
import SLD
import Pretty
import Substitution

interactive :: IO()
interactive = do
    putStrLn welcomeWaggon
    looping "" (Prog []) dfs -- loop  it
    putStrLn quitWaggon

looping :: String -> Prog -> Strategy -> IO()
looping s p strat = do
    x <- readFromUser -- read user input
    let (a, str)   = parseLine x
    let (rC, progr, gol, stratN, response) | a == 'l'  = evalActionLoad str 
                                           | a == 'r'  = evalActionLoad s
                                           | otherwise = evalAction a str
    case rC of  
       4 -> do -- solve a goal
              solveGoal p gol strat
              looping s p strat 
       3 -> return() -- byebye
       1 -> looping str progr strat -- loaded a Prog from File
       5 -> do
              putStrLn response
              looping s p stratN -- new Strat
       _ -> do -- print and loop -> error, help message
              putStrLn response     
              looping s p strat                      

solveGoal :: Prog -> Goal -> Strategy -> IO()
solveGoal p g s = do 
                    let a = solveWith p g s
                    printIfWanted a

printIfWanted :: [Subst] -> IO ()
printIfWanted s = case s of 
                   []     -> putStrLn ""
                   (s:rs) -> do 
                              putStrLn (pretty s) -- pretty output the substitution
                              c <- getChar        -- get next action
                              when (c == ';') $ printIfWanted rs -- as long as ';' is used, continue 
      
evalActionLoad :: String -> (Integer, Prog, Goal, Strategy, String)
evalActionLoad l = do 
                     let r = parseFile l -- parse from filepath
                     case r of
                       Left  leftE  -> (-1, Prog [], Goal [], dfs, leftE)
                       Right rightE -> (1, rightE :: Prog, Goal [], dfs, "Loaded.")

evalAction :: Char -> String -> (Integer, Prog, Goal, Strategy, String)
evalAction c l | c == 'h' = (2, Prog[], Goal [], dfs, helpWaggon)
               | c == 'q' = (3, Prog[], Goal [], dfs, "")
               | c == 'g' = let r = parse l -- parse a goal
                            in case r of
                                 Left leftE -> (-1, Prog [], Goal [], dfs, leftE)
                                 Right rightE -> (4, Prog[], rightE, dfs, "")
               | c == 's' = case l of 
                              "dfs" -> (5, Prog [], Goal [], dfs, "Strategy set to deepth-first search.")
                              "bfs" -> (5, Prog [], Goal [], bfs, "Strategy set to breadth-first search.")
                              _     -> (-1, Prog [], Goal [], bfs, "Error: Strategy not found.")
               | otherwise  = (-1, Prog [], Goal [], dfs, "Error: Something went Wrong, probably wrong Command.\nType :h for Help.")

parseLine :: String -> (Char, String)
parseLine (s:rs) | s == ':' = (head rs, dropWhile isSpace (tail rs)) -- parse the first char after : and remove whitespaces
                 | otherwise = ('g', s:rs) -- otherwise it should be a goal 


readFromUser :: IO String
readFromUser = putStrLn  "?- "
            >> getLine 

helpWaggon :: String 
helpWaggon = unlines 
  [
      "Commands available from the prompt:"
     ,"<goal>      Solves/proves the specified goal."
     ,":h          Shows this help message."
     ,":l <file>   Loads the specified file."
     ,":q          Exits the interactive environment."
     ,":r          Reloads the last loaded file."
     ,":s <strat>  Sets the specified search strategy"
     ,"            where <strat> is one of 'dfs', 'bfs'."
  ]

welcomeWaggon :: String
welcomeWaggon = unlines
  [ "##################################"
   ,"Welcome to our very simpel Prolog"
   ,"Type \":h\" for help."
   ,"##################################"
  ]

quitWaggon :: String
quitWaggon = unlines 
  [
      "Bye."
     ,"Tschüss."
     ,"Ciao."
  ]