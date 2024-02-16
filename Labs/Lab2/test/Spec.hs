import Lib

import Test.HUnit

oneOneCases :: [(String, [(Char, Int)])]
oneOneCases = [
    ("aaaabbbcccbb", [('a', 4), ('b', 3), ('c', 3), ('b', 2)]),
    ("a aa aaa aaaa", [('a', 1), (' ', 1), ('a', 2), (' ', 1), ('a', 3), (' ', 1), ('a', 4)]),
    ("abcdefgh", [('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1), ('f', 1), ('g', 1), ('h', 1)])
  ]

-- Функція тестування для першого завдання.
-- Для зручності зробив вивід номера тестового варіанта, назву
createOneTestCases :: (Eq a, Show a) => [Char] -> (t -> a) -> [(t, a)] -> [Test]
createOneTestCases baseName func testCases = [TestCase (assertEqual (baseName ++ " " ++ show i) y (func x)) | (i, (x, y)) <- zip [0..] testCases]

oneOneATests :: [Test]
oneOneATests = createOneTestCases "OneOneA" oneOneA oneOneCases

oneOneBTests :: [Test]
oneOneBTests = createOneTestCases "OneOneB" oneOneB oneOneCases

twoOneCases :: [(([Int], Int), [Int])]
twoOneCases = [
    (([1, 2, 3, 4, 5], 0), [1, 2, 3, 4, 5]),
    (([1, 2, 3, 4, 5], 1), [2, 3, 4, 5, 1]),
    (([1, 2, 3, 4, 5], 2), [3, 4, 5, 1, 2]),
    (([1, 2, 3, 4, 5], 3), [4, 5, 1, 2, 3]),
    (([1, 2, 3, 4, 5], 4), [5, 1, 2, 3, 4]),
    (([1, 2, 3, 4, 5], 5), [1, 2, 3, 4, 5]),
    (([1, 2, 3, 4, 5], 6), [2, 3, 4, 5, 1]),
    (([1, 2, 3, 4, 5], 7), [3, 4, 5, 1, 2]),
    (([1, 2, 3, 4, 5], 8), [4, 5, 1, 2, 3]),
    (([1, 2, 3, 4, 5], 9), [5, 1, 2, 3, 4]),
    (([1, 2, 3, 4, 5], 10), [1, 2, 3, 4, 5])
  ]

-- Функція тестування для другого завдання.
createTwoTestCases :: (Eq a, Show a) => String -> (c -> b -> a) -> [((c, b), a)] -> [Test]
createTwoTestCases baseName func testCases = [TestCase (assertEqual (baseName ++ " " ++ show i) (func x y) z) | (i, ((x, y), z)) <- zip [0..] testCases]

-- Створюємо тести для twoTenA
twoOneATest :: [Test]
twoOneATest = createTwoTestCases "twoTenA" twoOneA twoOneCases

-- Створюємо тести для twoTenB
twoOneBTest :: [Test]
twoOneBTest = createTwoTestCases "twoTenB" twoOneB twoOneCases

-- Створюємо тестовий список
tests :: Test
tests = TestList (oneOneATests ++ oneOneBTests ++ twoOneATest ++ twoOneBTest)

-- Головна функція, виводить результати тестування.
main :: IO ()
main  = do
    result <- runTestTT tests
    print (if failures result > 0 then "Failure" else "Success")