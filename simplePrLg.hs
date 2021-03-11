module Main where
import Substitution
import Rename
import Unifikation

import qualified Interactive

-- start programm
main :: IO ()
main = do _ <-testAllModules -- look down
          Interactive.start -- yes it starts the program

-- test all modules with tests
testAllModules :: IO ()
testAllModules = do a <- testAllUnifikation
                    b <-testAllSubstitution
                    c <- testAllRename
                    if a && b && c then putStrLn "Done" else putStrLn "Fail."