import Lib

import Test.HUnit
import qualified System.Exit as Exit

main :: IO ()
main  = do
    result <- runTestTT tests
    print (if failures result > 0 then "Failure" else "Success")

oneOneCases = [
    ([1, 2, 2, 3, 4, 5, 5], [1, 2, 3, 4, 5]),
    ([1, 1, 2, 2, 1, 1, 2, 2, 2, 3, 1, 1], [1, 2, 1, 2, 3, 1]),
    ([3, -1, -1, -2, 0, 0, 4, 5, 5, 4], [3, -1, -2, 0, 4, 5, 4])
  ]

createTestCases baseName testCases = [TestCase (assertEqual (baseName ++ " " ++ show i) (oneOne x) y) | (i, (x, y)) <- zip [0..] testCases]

tests :: Test
tests = TestList (createTestCases "OneOne" oneOneCases)