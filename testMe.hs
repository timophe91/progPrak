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
testAllModules = do _ <-testAllUnifikation
                    _ <-testAllSubstitution
                    _ <- testAllRename
                    putStrLn "Done"
                     
