module Main (main) where

import AcceptanceTests (acceptanceTests)
import RefactorTests (refactorTests)
import RejectionTests (rejectionTests)
import System.Exit (exitFailure, exitSuccess)
import TestUtils (report)

main :: IO ()
main = do
  results <- sequence (acceptanceTests ++ rejectionTests ++ refactorTests)
  mapM_ report results
  putStrLn ""
  let total = length results
      failed = length (filter (not . snd) results)
  if failed == 0
    then putStrLn (show total ++ " tests passed.") >> exitSuccess
    else
      putStrLn (show failed ++ " of " ++ show total ++ " tests failed.")
        >> exitFailure
