-- Enter your code here. Read input from STDIN. Print output to STDOUT

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (replicateM)

runTest :: [[String]] -> String
runTest testCases = toResult $ foldl evalCase (Just S.empty) testCases
  where evalCase (Just m) [x, y] = if S.member x m
                                     then Nothing
                                     else Just $ S.insert x m
        toResult Nothing = "NO"
        toResult (Just _) = "YES"

test :: Int -> IO ()
test testCaseCount = do
  pairCountInput <- getLine
  let pairCount = (read pairCountInput :: Int)
  inputs <- replicateM pairCount getLine >>= return . map words
  print $ runTest inputs

main :: IO()
main = do
  testCasesInput <- getLine
  let testCaseCount = read testCasesInput :: Int
  test testCaseCount
