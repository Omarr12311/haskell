import Assignment2
import Types
import Data.Maybe (fromJust)

-- Test helper
testCase :: (Show a, Eq a) => String -> a -> a -> IO Bool
testCase name expected actual = do
    if expected == actual
        then do
            putStrLn $ "[PASS] " ++ name
            return True
        else do
            putStrLn $ "[FAIL] " ++ name
            putStrLn $ "  Expected: " ++ show expected
            putStrLn $ "  Got:      " ++ show actual
            return False

main :: IO ()
main = do
    putStrLn "=========================================="
    putStrLn "Testing Assignment 2 (Questions 1-5)"
    putStrLn "=========================================="

    results <- sequence [
        -- Question 1: encodeWord tests
        do
            putStrLn "\n--- Question 1: encodeWord ---"
            r1 <- testCase "encodeWord empty string"
                []
                (encodeWord morseTable "")
            r2 <- testCase "encodeWord single char 'A'"
                [Beep,Silence,Beep,Beep,Beep,Silence]
                (encodeWord morseTable "A")
            r3 <- testCase "encodeWord '0'"
                [Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence]
                (encodeWord morseTable "0")
            r4 <- testCase "encodeWord 'HELLO'"
                [Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence]
                (encodeWord morseTable "HELLO")
            r5 <- testCase "encodeWord 'WORLD'"
                [Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence]
                (encodeWord morseTable "WORLD")
            return (r1 && r2 && r3 && r4 && r5),

        -- Question 1: encodeWords tests
        do
            putStrLn "\n--- Question 1: encodeWords ---"
            r1 <- testCase "encodeWords empty list"
                []
                (encodeWords morseTable [])
            r2 <- testCase "encodeWords [\"007\"]"
                [Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence]
                (encodeWords morseTable ["007"])
            r3 <- testCase "encodeWords [\"HI\",\"THERE\"]"
                [Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence]
                (encodeWords morseTable ["HI","THERE"])
            return (r1 && r2 && r3),

        -- Question 1: encodeText tests
        do
            putStrLn "\n--- Question 1: encodeText ---"
            r1 <- testCase "encodeText 'WORD'"
                [Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence]
                (encodeText morseTable "WORD")
            r2 <- testCase "encodeText 'HI THERE'"
                [Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence]
                (encodeText morseTable "HI THERE")
            r3 <- testCase "encodeText 'THIS IS A TEST'"
                [Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence]
                (encodeText morseTable "THIS IS A TEST")
            return (r1 && r2 && r3),

        -- Question 2: decodeText tests
        do
            putStrLn "\n--- Question 2: decodeText ---"
            let encoded = encodeText morseTable "THIS IS A TEST"
            r1 <- testCase "decodeText (encodeText 'THIS IS A TEST')"
                "THIS IS A TEST"
                (decodeText morseTable encoded)
            let encoded2 = encodeText morseTable "HELLO WORLD"
            r2 <- testCase "decodeText (encodeText 'HELLO WORLD')"
                "HELLO WORLD"
                (decodeText morseTable encoded2)
            let encoded3 = encodeText morseTable "A"
            r3 <- testCase "decodeText (encodeText 'A')"
                "A"
                (decodeText morseTable encoded3)
            return (r1 && r2 && r3),

        -- Question 3: decodeTextWithTree tests
        do
            putStrLn "\n--- Question 3: decodeTextWithTree ---"
            let morseTree = Branch Nothing (Branch (Just 'E') (Branch (Just 'I') (Branch (Just 'S') (Branch (Just 'H') (Branch (Just '5') Empty Empty) (Branch (Just '4') Empty Empty)) (Branch (Just 'V') Empty (Branch (Just '3') Empty Empty))) (Branch (Just 'U') (Branch (Just 'F') Empty Empty) (Branch Nothing Empty (Branch (Just '2') Empty Empty)))) (Branch (Just 'A') (Branch (Just 'R') (Branch (Just 'L') Empty Empty) Empty) (Branch (Just 'W') (Branch (Just 'P') Empty Empty) (Branch (Just 'J') Empty (Branch (Just '1') Empty Empty))))) (Branch (Just 'T') (Branch (Just 'N') (Branch (Just 'D') (Branch (Just 'B') (Branch (Just '6') Empty Empty) Empty) (Branch (Just 'X') Empty Empty)) (Branch (Just 'K') (Branch (Just 'C') Empty Empty) (Branch (Just 'Y') Empty Empty))) (Branch (Just 'M') (Branch (Just 'G') (Branch (Just 'Z') (Branch (Just '7') Empty Empty) Empty) (Branch (Just 'Q') Empty Empty)) (Branch (Just 'O') (Branch Nothing (Branch (Just '8') Empty Empty) Empty) (Branch Nothing (Branch (Just '9') Empty Empty) (Branch (Just '0') Empty Empty)))))
            let encoded = encodeText morseTable "THIS IS ANOTHER TEST"
            r1 <- testCase "decodeTextWithTree morseTree (encodeText 'THIS IS ANOTHER TEST')"
                "THIS IS ANOTHER TEST"
                (decodeTextWithTree morseTree encoded)
            let encoded2 = encodeText morseTable "HELLO"
            r2 <- testCase "decodeTextWithTree morseTree (encodeText 'HELLO')"
                "HELLO"
                (decodeTextWithTree morseTree encoded2)
            return (r1 && r2),

        -- Question 4: ramify tests
        do
            putStrLn "\n--- Question 4: ramify ---"
            let morseTree = Branch Nothing (Branch (Just 'E') (Branch (Just 'I') (Branch (Just 'S') (Branch (Just 'H') (Branch (Just '5') Empty Empty) (Branch (Just '4') Empty Empty)) (Branch (Just 'V') Empty (Branch (Just '3') Empty Empty))) (Branch (Just 'U') (Branch (Just 'F') Empty Empty) (Branch Nothing Empty (Branch (Just '2') Empty Empty)))) (Branch (Just 'A') (Branch (Just 'R') (Branch (Just 'L') Empty Empty) Empty) (Branch (Just 'W') (Branch (Just 'P') Empty Empty) (Branch (Just 'J') Empty (Branch (Just '1') Empty Empty))))) (Branch (Just 'T') (Branch (Just 'N') (Branch (Just 'D') (Branch (Just 'B') (Branch (Just '6') Empty Empty) Empty) (Branch (Just 'X') Empty Empty)) (Branch (Just 'K') (Branch (Just 'C') Empty Empty) (Branch (Just 'Y') Empty Empty))) (Branch (Just 'M') (Branch (Just 'G') (Branch (Just 'Z') (Branch (Just '7') Empty Empty) Empty) (Branch (Just 'Q') Empty Empty)) (Branch (Just 'O') (Branch Nothing (Branch (Just '8') Empty Empty) Empty) (Branch Nothing (Branch (Just '9') Empty Empty) (Branch (Just '0') Empty Empty)))))
            r1 <- testCase "ramify morseTable == expected morseTree"
                morseTree
                (ramify morseTable)
            return r1,

        -- Question 5: tabulate tests
        do
            putStrLn "\n--- Question 5: tabulate ---"
            let morseTree = Branch Nothing (Branch (Just 'E') (Branch (Just 'I') (Branch (Just 'S') (Branch (Just 'H') (Branch (Just '5') Empty Empty) (Branch (Just '4') Empty Empty)) (Branch (Just 'V') Empty (Branch (Just '3') Empty Empty))) (Branch (Just 'U') (Branch (Just 'F') Empty Empty) (Branch Nothing Empty (Branch (Just '2') Empty Empty)))) (Branch (Just 'A') (Branch (Just 'R') (Branch (Just 'L') Empty Empty) Empty) (Branch (Just 'W') (Branch (Just 'P') Empty Empty) (Branch (Just 'J') Empty (Branch (Just '1') Empty Empty))))) (Branch (Just 'T') (Branch (Just 'N') (Branch (Just 'D') (Branch (Just 'B') (Branch (Just '6') Empty Empty) Empty) (Branch (Just 'X') Empty Empty)) (Branch (Just 'K') (Branch (Just 'C') Empty Empty) (Branch (Just 'Y') Empty Empty))) (Branch (Just 'M') (Branch (Just 'G') (Branch (Just 'Z') (Branch (Just '7') Empty Empty) Empty) (Branch (Just 'Q') Empty Empty)) (Branch (Just 'O') (Branch Nothing (Branch (Just '8') Empty Empty) Empty) (Branch Nothing (Branch (Just '9') Empty Empty) (Branch (Just '0') Empty Empty)))))
            -- Test that ramify . tabulate = id
            r1 <- testCase "ramify (tabulate morseTree) == morseTree"
                morseTree
                (ramify (tabulate morseTree))
            -- Additional test: encodeText and decodeText should work with tabulated tree
            let table = tabulate morseTree
            let encoded = encodeText table "TEST"
            r2 <- testCase "decodeText (tabulate morseTree) (encodeText (tabulate morseTree) 'TEST') == 'TEST'"
                "TEST"
                (decodeText table encoded)
            return (r1 && r2)
        ]

    putStrLn "\n=========================================="
    putStrLn "Test Summary"
    putStrLn "=========================================="
    let passed = length (filter id results)
    let total = length results
    putStrLn $ "Passed: " ++ show passed ++ " / " ++ show total ++ " test groups"

    if all id results
        then putStrLn "\n*** ALL TESTS PASSED! ***"
        else putStrLn "\n*** SOME TESTS FAILED - Please review the output above ***"
