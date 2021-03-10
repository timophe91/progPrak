module Interactive where

import Control.Monad
import Data.Char
import Parser
import Data.Either
import Type
import SLD
import Pretty
import Substitution

-- Interactive ERPL for a simple Prolog
interactive :: IO()
interactive = do
  putStrLn welcomeWaggon
  readCommand dfs "" (Left "Program started, no File Loaded.")

      where
        -- Read a command and pipe it to eval
        readCommand :: Strategy -> String ->  Either String Prog -> IO()
        readCommand  strat filePath eProg = do putStr "?- "
                                               c <- getLine -- read from console
                                               eval strat c filePath eProg

        eval :: Strategy -> String -> String -> Either String Prog -> IO()
        -- call the help Waggon
        eval strat' ":h"   filePath' eProg' = do putStrLn helpWaggon
                                                 readCommand strat' filePath' eProg' -- loop
        -- reload prog
        eval strat' ":r"   filePath' eProg' = do loadedFile <- parseFile filePath'
                                                 case loadedFile of
                                                   Left  errStr''' -> do putStrLn errStr''' -- error but keep Path and failed Prog
                                                                         readCommand strat' filePath' (Left  errStr''') -- loop with new error
                                                   Right eProg'''  -> do putStrLn "Reloaded."
                                                                         readCommand strat' filePath' (Right eProg''') -- loop with reloaded prog
        -- end program
        eval _      ":q"   _         _      = putStrLn quitWaggon -- quit
        -- evaluate user input
        eval strat' s      filePath' eProg' = let (action:userInput) = words s -- split words by space or newline
                                              in case length userInput of 
                                                   0 ->  let evalGoal = parse action -- the is only one command, and its the goal
                                                         in case evalGoal of -- check Goal for error
                                                              Left  errStr  -> do putStrLn "Error Parsing Goal:"
                                                                                  putStrLn errStr -- error in parsing Goal
                                                                                  readCommand strat' filePath' eProg' -- loop 
                                                              Right rdGoal -> case eProg' of -- check if Prog is loaded correctly 
                                                                                Left  errStr' -> do putStrLn "Error Parsing Prog:"
                                                                                                    putStrLn errStr'
                                                                                                    readCommand strat' filePath' eProg' -- loop
                                                                                Right rdProg  -> do printSolutions (solveWith rdProg rdGoal strat')
                                                   1 -> case action of
                                                          -- load a file
                                                          ":l" -> do loadedFile <- parseFile (head userInput)
                                                                     case loadedFile of
                                                                       Left  errStr'' -> do putStrLn errStr'' -- error but keep Path and failed Prog
                                                                                            readCommand strat' (head userInput) (Left  errStr'') -- loop with loaded error and failed path
                                                                       Right eProg''  -> do putStrLn "Loaded."
                                                                                            readCommand strat' (head userInput) (Right eProg'') -- loop with new prog and succeded path
                                                          -- change the strategy
                                                          ":s" | head userInput == "dfs" -> do putStrLn "Strategy set to depth-first search."
                                                                                               readCommand dfs filePath' eProg' -- loop with dfs as strat
                                                               | head userInput == "bfs" -> do putStrLn "Strategy set to breadth-first search."
                                                                                               readCommand bfs filePath' eProg' -- loop with bfs as strat
                                                               | otherwise               -> do putStrLn "The Strategy u r looking for was not found.\nType ':h' for help."
                                                                                               readCommand strat' filePath' eProg' -- loop with old strat
                                                          -- corrent number of commands, but command not available
                                                          _    -> do putStrLn "Command not found. \nType ':h' for help."
                                                                     readCommand strat' filePath' eProg' -- loop
                                                  -- wrong number of commands and input
                                                   _    -> do putStrLn "Command not found. \nType ':h' for help."
                                                              readCommand strat' filePath' eProg' -- loop

-- Pretty Printing the Solutions
printSolutions :: [Subst] -> IO ()
printSolutions s = case s of 
                   []     -> putStrLn "No more Solutions found."
                   (s:rs) -> do 
                              putStrLn (pretty s) -- pretty output the substitution
                              c <- getChar        -- get next action
                              when (c == ';') $ printSolutions rs -- as long as ';' is used, continue 
-- just the help message
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
     ,"            default is 'dfs'"
  ]

-- welcome message
welcomeWaggon :: String
welcomeWaggon = unlines
  [ "##################################"
   ,"Welcome to our very simpel Prolog"
   ,"Type \":h\" for help."
   ,"##################################"
  ]

-- quit message
quitWaggon :: String
quitWaggon = unlines 
  [
      "Bye."
     ,"Tschüss."
     ,"Ciao."
  ]