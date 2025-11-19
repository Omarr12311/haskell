-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

module Assignment2 (encodeWord , encodeWords , encodeText ,
                    decodeText ,
                    decodeTextWithTree ,
                    ramify ,
                    tabulate ,
                    tree) where

import Types
import Data.List

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
encodeWord :: Table -> String -> Code
encodeWord _ [] = []
encodeWord table [c] = case lookup c table of
    Just code -> code
    Nothing -> []
encodeWord table (c:cs) = case lookup c table of
    Just code -> code ++ shortGap ++ encodeWord table cs
    Nothing -> encodeWord table cs

encodeWords :: Table -> [String] -> Code
encodeWords _ [] = []
encodeWords table [c] = encodeWord table c
encodeWords table (c:cs) = encodeWord table c ++ mediumGap ++ encodeWords table cs 

split :: Eq a => [a] -> [a] -> [[a]]
split _ []   = []
split sep xs = reverse (map reverse (go [] (reverse xs)))
  where
    go acc [] =
      if null acc
        then []
        else [acc]

    go acc ys =
      case stripPrefix (reverse sep) ys of
        Just rest ->
          if null acc
            then go [] rest      
            else acc : go [] rest
        Nothing ->
          case ys of
            (y:ys') -> go (y:acc) ys'

    stripPrefix [] zs = Just zs
    stripPrefix _  [] = Nothing
    stripPrefix (p:ps) (z:zs)
      | p == z    = stripPrefix ps zs
      | otherwise = Nothing




        
encodeText :: Table -> String -> Code
encodeText table text = intercalate mediumGap encodedWords
  where
    wordList = split " " text
    encodedWords = map (encodeWord table) wordList


    
{- Question 2 -}
decodeText :: Table -> Code -> String
decodeText _ [] = ""
decodeText table code = unwords decodedWords
  where
    wordCodes = case split (replicate 7 Silence) code of
                    [] -> [code] 
                    ws -> filter (\x -> not (null x)) ws
    
    decodedWords = map decodeWord wordCodes
    decodeWord :: Code -> String
    decodeWord wordCode =
        case split (replicate 3 Silence) wordCode of
            [] -> tryDirectDecode wordCode 
            parts -> 
                let nonEmptyParts = filter (\x -> not (null x)) parts
                    remainingCode = drop (sum (map length nonEmptyParts) + (length nonEmptyParts - 1) * 3) wordCode
                    allParts = if null remainingCode 
                               then nonEmptyParts
                               else nonEmptyParts ++ [remainingCode]
                in map decodeLetter allParts

    tryDirectDecode :: Code -> String
    tryDirectDecode c = case lookup c [(code, char) | (char, code) <- table] of
                            Just char -> [char]
                            Nothing -> ""
    
    decodeLetter :: Code -> Char
    decodeLetter letterCode = 
        case lookup (letterCode ++ [Silence]) [(code, char) | (char, code) <- table] of
            Just c -> c
            Nothing -> case lookup letterCode [(code, char) | (char, code) <- table] of
                    Just c -> c
                    Nothing -> '?'
                    
{- Question 3 -}
decodeTextWithTree :: Tree -> Code -> String
decodeTextWithTree _ [] = ""
decodeTextWithTree tree code = unwords decodedWords
  where
    wordCodes = filter (not . null) (split (replicate 7 Silence) code)
    decodedWords = map decodeWord wordCodes
    
    decodeWord :: Code -> String  
    decodeWord wordCode =
        let letterCodes = filter (not . null) (split (replicate 3 Silence) wordCode)
        in map decodeLetter letterCodes
    
    decodeLetter :: Code -> Char
    decodeLetter letterCode = decodeFromTree tree letterCode
        
    decodeFromTree :: Tree -> Code -> Char
    decodeFromTree Empty _ = '?'
    decodeFromTree (Branch mc _ _) [] = maybe '?' id mc
    decodeFromTree (Branch mc _ _) (Silence:_) = maybe '?' id mc
    decodeFromTree (Branch _ left _) (Beep:Silence:rest) = 
        decodeFromTree left rest
    decodeFromTree (Branch _ left _) (Beep:[]) = 
        decodeFromTree left []
    decodeFromTree (Branch _ _ right) (Beep:Beep:Beep:Silence:rest) = 
        decodeFromTree right rest
    decodeFromTree (Branch _ _ right) (Beep:Beep:Beep:[]) = 
        decodeFromTree right []
    decodeFromTree _ _ = '?'

{- Question 4 -}
ramify :: Table -> Tree
ramify table = foldl insertChar Empty table
  where
    insertChar :: Tree -> (Char, Code) -> Tree
    insertChar tree (char, code) = insertAtPath tree char (codeToPath code)
    
    codeToPath :: Code -> [Bool]
    codeToPath [] = []
    codeToPath (Beep:Silence:rest) = False : codeToPath rest
    codeToPath (Beep:Beep:Beep:Silence:rest) = True : codeToPath rest 
    codeToPath _ = [] 
    
    insertAtPath :: Tree -> Char -> [Bool] -> Tree
    insertAtPath Empty char [] = Branch (Just char) Empty Empty
    insertAtPath Empty char (False:rest) = 
        Branch Nothing (insertAtPath Empty char rest) Empty 
    insertAtPath Empty char (True:rest) = 
        Branch Nothing Empty (insertAtPath Empty char rest) 
    insertAtPath (Branch mc left right) char [] = 
        Branch (Just char) left right
    insertAtPath (Branch mc left right) char (False:rest) = 
        Branch mc (insertAtPath left char rest) right 
    insertAtPath (Branch mc left right) char (True:rest) = 
        Branch mc left (insertAtPath right char rest)

{- Question 5 -}
tabulate :: Tree -> Table
tabulate tree = collectPaths tree []
  where
    collectPaths :: Tree -> Code -> Table
    collectPaths Empty _ = []
    collectPaths (Branch mc left right) pathCode =
        let currentEntry = case mc of
                Just c -> [(c, pathCode)]
                Nothing -> []
            leftEntries = collectPaths left (pathCode ++ dit)
            rightEntries = collectPaths right (pathCode ++ dah)
        in currentEntry ++ leftEntries ++ rightEntries

{- Question 6 -}
brackets :: Bracket -> String
brackets (Round ts) = "(" ++ concat [brackets t | t <- ts] ++ ")"
brackets (Curly ts) = "{" ++ concat [brackets t | t <- ts] ++ "}"

tree :: String -> Maybe Bracket
tree = undefined

isWellBracketed :: String -> Bool
isWellBracketed xs = case tree xs of
                      Nothing -> False
                      Just _  -> True
