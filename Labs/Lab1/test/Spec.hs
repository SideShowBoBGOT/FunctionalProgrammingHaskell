import Lib

import Test.HUnit

main :: IO ()
main  = do
    result <- runTestTT tests
    print (if failures result > 0 then "Failure" else "Success")

oneTwoCases :: [(String, [(Char, Int)])]
oneTwoCases = [
    ("aaaabbbcccbb", [('a', 4), ('b', 3), ('c', 3), ('b', 2)]),
    ("a aa aaa aaaa", [('a', 1), (' ', 1), ('a', 2), (' ', 1), ('a', 3), (' ', 1), ('a', 4)]),
    ("abcdefgh", [('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1), ('f', 1), ('g', 1), ('h', 1)])
  ]

createTestCases :: (Eq a, Show a) => [Char] -> (t -> a) -> [(t, a)] -> [Test]
createTestCases baseName func testCases = [TestCase (assertEqual (baseName ++ " " ++ show i) y (func x)) | (i, (x, y)) <- zip [0..] testCases]

twoOneCases :: [([Integer], (Int, Int, [Integer]))]
twoOneCases = [
    ([99, 101, 1, 2, 66], (5, 2, [99, 101, 1, 2, 66])), -- /home/sideshowbobgot/Downloads/JMeter.docx -- invalid indexes 5 > 2;
    ([99, 101, 1, 2, 66], (-2, -1, [99, 101, 1, 2, 66])), -- x e [i; j], j < 0;
    ([99, 101, 1, 2, 66], (5, 7, [99, 101, 1, 2, 66])), -- i > length list;
    ([99, 101, 1, 66], (3, 3, [99, 101, 1, 2, 66])), -- i == j, i >= 0
    ([99, 66], (1, 3, [99, 101, 1, 2, 66])), -- valid [i; j] intersect range [0; length l]
    ([66], (-999, 3, [99, 101, 1, 2, 66])) -- valid [i; j] intersect range [0; length l]
  ]

twoTwoCases = [
    ([5, 1, 2, 3, 4], 1),
    ([5, 1, 2, 3, 4], 6),
    ([5, 1, 2, 3, 4], 11),
    ([2, 3, 4, 5, 1], -1),
    ([2, 3, 4, 5, 1], -6),
    ([2, 3, 4, 5, 1], -11),
    ([1, 2, 3, 4, 5], 0),
    ([1, 2, 3, 4, 5], 5),
    ([1, 2, 3, 4, 5], 10),
    ([4, 5, 1, 2, 3], 2),
    ([4, 5, 1, 2, 3], 7),
    ([4, 5, 1, 2, 3], 12),
    ([3, 4, 5, 1, 2], -2),
    ([3, 4, 5, 1, 2], -7),
    ([3, 4, 5, 1, 2], -12)
  ]

twoTwoTests = [TestCase (assertEqual ("TwoTwo " ++ show index) expected (twoTwo [1, 2, 3, 4, 5] n)) | (index, (expected, n)) <- zip [0..] twoTwoCases]

tests :: Test
tests = TestList ((createTestCases "OneTwo" oneTwo oneTwoCases) ++ twoTwoTests)