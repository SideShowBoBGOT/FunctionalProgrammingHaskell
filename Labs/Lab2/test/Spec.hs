import Lib

import Test.HUnit

oneOneCases :: [(String, [(Char, Int)])]
oneOneCases = [
    ("aaaabbbcccbb", [('a', 4), ('b', 3), ('c', 3), ('b', 2)]),
    ("a aa aaa aaaa", [('a', 1), (' ', 1), ('a', 2), (' ', 1), ('a', 3), (' ', 1), ('a', 4)]),
    ("abcdefgh", [('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1), ('f', 1), ('g', 1), ('h', 1)])
  ]

createTestCases :: (Eq a, Show a) => [Char] -> (t -> a) -> [(t, a)] -> [Test]
createTestCases baseName func testCases = [TestCase (assertEqual (baseName ++ " " ++ show i) y (func x)) | (i, (x, y)) <- zip [0..] testCases]

tests :: Test
tests = TestList (createTestCases "OneOne" oneOneA oneOneCases)

main :: IO ()
main  = do
    result <- runTestTT tests
    print (if failures result > 0 then "Failure" else "Success")