module Interactive where

import Control.Monad
import Data.Char
import Parser
import Data.Either
interactive :: IO()
interactive = do
    putStrLn welcomeWaggon
    looping -- loop  it
    putStrLn quitWaggon 
    return()

looping :: IO()
looping = do
    x <- readFromUser -- read user input
    -- TODO: eval etc.
    --unless (x /= ":q") -- do the loop until it quits
    --  $ printResponse (evalAction parseLine) >> looping
      

evalAction :: Char -> String -> String
evalAction c l | c == 'h' = helpWaggon
               | c == 'l' = let r = parseFile l -- parse ´from filepath
                            in if not (null (fromRight r)) then "Loaded." else fromLeft r -- hm
               | c == 'g' = let g = parse l -- parse a goal
                            in "TODO: Implement handle goal" 
               | c == 'q' = "Please Quit with ':q'" -- irgendwas stimmte nicht.
                             

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
     ;"Ciao."
  ]