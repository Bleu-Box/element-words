-- This program examines a given string and returns the most similar string that can be
-- made using just element name abbreviations (i.e. He, F, Cl)

import Data.Char (toLower)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.List (minimumBy)

main :: IO ()
main = do f <- readFile "data/elementlist.csv"
          putStrLn "Element Name Converter: Convert any text to a string made of chemical element abbreviations. \n\
                   \\tEnter :q to quit."
          run f

-- get just the abbreviations from the file (which also has elements' numbers and full names)
abbrevs :: String -> [String]
abbrevs = map ((!! 1) . (splitOn ",")) . lines

-- this is the interface to the converter
run :: String -> IO ()
run f = do putStr "Enter text to convert: "
           input <- getLine
           if input == ":q"
             then return ()
             else do let names = abbrevs f
                     putStrLn $ elementName input names
                     run f

-- this function that a word from element names
elementName :: String -> [String] -> String
elementName [] _ = []
elementName (x:(y:[])) src = foldr (\a b -> closestWord [x, y] a b) " " src
elementName (x:[]) src = foldr (\a b -> closestWord [x] a b) " " src
elementName (x:(y:xs)) src | levenshteinInsensitive [x] headMatch < levenshteinInsensitive [x, y] chunkMatch
                               = headMatch ++ elementName (y:xs) src 
                           | otherwise = chunkMatch ++ elementName xs src
  where headMatch = elementName [x] src
        chunkMatch = elementName [x, y] src

-- get the string closest to the target string
closestWord :: String -> String -> String -> String
closestWord target a b = minimumBy (comparing $ levenshteinInsensitive target) [a, b]

-- this version of levenshtein distance ignores case by lowercasing its inputs
levenshteinInsensitive :: String -> String -> Int
levenshteinInsensitive a b = levenshtein (map toLower a) (map toLower b)

-- get the Levenshtein distance between two strings
levenshtein :: String -> String -> Int
levenshtein a b = levenshtein' (length a) (length b)
  where levenshtein' i j = if min i j == 0 then max i j
                           else minimum [1 + levenshtein' (i-1) j,
                                         1 + levenshtein' i (j-1),
                                         indicator i j + levenshtein' (i-1) (j-1)]
        indicator i j | a !! (i-1) == b !! (j-1) = 0
                      | otherwise = 1
