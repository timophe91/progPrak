module Main where
import Substitution
import Rename
import Unifikation

import qualified Interactive

main :: IO ()
main = do _ <-testAllModules
          Interactive.start

-- test all modules with tests
testAllModules :: IO ()
testAllModules = do a <- testAllUnifikation
                    b <-testAllSubstitution
                    c <- testAllRename
                    if a && b && c then putStrLn "Done" else putStrLn "Fail."