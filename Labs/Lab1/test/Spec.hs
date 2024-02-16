import Lib

import Test.HUnit

-- Функція тестування для першого завдання.
-- Для зручності зробив вивід номера тестового варіанта, назву
createOneTestCases :: (Eq a, Show a) => String -> (t -> a) -> [(t, a)] -> [Test]
createOneTestCases baseName func testCases = [TestCase (assertEqual (baseName ++ " " ++ show i) (func x) y) | (i, (x, y)) <- zip [0..] testCases]

-- Тестові варіанти для першого завдання. Перший елемент кортежу - аргумент,
-- друге - очікуване значення тесту.
oneTenCases :: [([Int], Int)]
oneTenCases = [
    ([1..2], 1),
    ([1..3], 2),
    ([1..4], 3)
  ]

-- Створюємо тести для oneTestA
oneTenATest :: [Test]
oneTenATest = createOneTestCases "oneTenA" oneTenA oneTenCases

-- Створюємо тести для oneTestB
oneTenBTest :: [Test]
oneTenBTest = createOneTestCases "oneTenB" oneTenB oneTenCases

-- Тестові варіанти для другого завдання. Перший елемент кортежу -
-- це кортеж аргументів, другий - очікуване значення тесту.
twoTenCases :: [(([Int], Int), [Int])]
twoTenCases = [
    (([1, 2, 3, 4, 5], 0), [1, 2, 3, 4, 5]),
    (([1, 2, 3, 4, 5], 1), [5, 1, 2, 3, 4]),
    (([1, 2, 3, 4, 5], 2), [4, 5, 1, 2, 3]),
    (([1, 2, 3, 4, 5], 3), [3, 4, 5, 1, 2]),
    (([1, 2, 3, 4, 5], 4), [2, 3, 4, 5, 1]),
    (([1, 2, 3, 4, 5], 5), [1, 2, 3, 4, 5]),
    (([1, 2, 3, 4, 5], 6), [5, 1, 2, 3, 4]),
    (([1, 2, 3, 4, 5], 7), [4, 5, 1, 2, 3]),
    (([1, 2, 3, 4, 5], 8), [3, 4, 5, 1, 2]),
    (([1, 2, 3, 4, 5], 9), [2, 3, 4, 5, 1]),
    (([1, 2, 3, 4, 5], 10), [1, 2, 3, 4, 5])
  ]

-- Функція тестування для другого завдання.
createTwoTestCases :: (Eq a, Show a) => String -> (c -> b -> a) -> [((c, b), a)] -> [Test]
createTwoTestCases baseName func testCases = [TestCase (assertEqual (baseName ++ " " ++ show i) (func x y) z) | (i, ((x, y), z)) <- zip [0..] testCases]

-- Створюємо тести для twoTenA
twoTenATest :: [Test]
twoTenATest = createTwoTestCases "twoTenA" twoTenA twoTenCases

-- Створюємо тести для twoTenB
twoTenBTest :: [Test]
twoTenBTest = createTwoTestCases "twoTenB" twoTenB twoTenCases

-- Створюємо тестовий список
tests :: Test
tests = TestList (oneTenATest ++ oneTenBTest ++ twoTenATest ++ twoTenBTest)

-- Головна функція, виводить результати тестування.
main :: IO ()
main  = do
    result <- runTestTT tests
    print (if failures result > 0 then "Failure" else "Success")