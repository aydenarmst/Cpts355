-- CptS 355 - Lab 2 (Haskell) - Fall 2022
-- Name: Ayden Armstrong 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use lambda-case" #-}


module Lab2
     where
import Distribution.Simple.Utils (xargs)
import Data.String (IsString)



-- 1
{- (a) merge2 -}
-- Takes two lists and returns the merged list of both 
merge2 :: [a] -> [a] -> [a]
merge2 [] [] = []
merge2 [] (y:ys) = y:ys
merge2 (x:xs) [] = x:xs
merge2 (x:xs) (y:ys) = x:y:merge2 xs ys

{- (b) merge2Tail -}
merge2Tail :: [a] -> [a] -> [a]
merge2Tail list1 list2 = mergeHelper list1 list2 []
     where
          mergeHelper [] ys acc = acc ++ ys
          mergeHelper xs [] acc = acc ++ xs
          mergeHelper (x:xs) (y:ys) acc = mergeHelper xs ys (acc ++ [x,y])


 {- (c) merge2Tail' -}
 -- I assume by the reblic that explict recursion is meaning to not call MergeN as recursion? 
mergeN :: [[a]] -> [a]
mergeN [] = []
mergeN xs = foldl1 merge2 xs


--2

{- (a) -}
-- Count
--Takes a value and a lsit aas input and it count the number of occurances of the value in that input list
count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x xs = sum $ map (const 1) $ filter (==x) xs


{- (b) -}
-- Histogram 

histogram :: Eq a => [a] -> [(a, Int)]
histogram xs  = map(\x -> (x, count x xs)) (unique xs)
     where
          unique :: Eq a => [a] -> [a]
          unique [] = []
          unique xs = foldl (\acc x -> if x `elem` acc -- if x is an element of acc
                              then acc -- then return acc without the element
                              else x : acc ) [] xs -- else return acc with the element added to the end


--3 
{- (a) -}
--ConcatAll
-- concatAll [["enrolled"," ","in"," "],["CptS","-","355"],[" ","and"," "],["CptS","-","322"]]
concatAll :: [[String]] -> String
concatAll list = concat (map concat list)


{- (b) -}
-- new datatype
data AnEither = AString String | AnInt Int
               deriving (Show, Read, Eq)



-- concat2Either
 --concat2Either [[AString "enrolled", AString " ", AString "in", AString " "],[AString "CptS", AString "-", AnInt 355], [AString " ", AString "and", AString " "], [AString "CptS", AString "-", AnInt 322]]
concat2Either :: [[AnEither]] -> AnEither
concat2Either list = either_concat (map either_concat list)
     where
          either_concat :: [AnEither] -> AnEither
          either_concat [] = AString ""
          either_concat list = AString (foldr (\element mainAString -> case element of -- case statement to check if the element is of Astring or AInt
                                                            AString string -> string ++ mainAString -- if it is a string then add it to the main Astring
                                                            AnInt intToString -> show intToString ++ mainAString) "" list) -- if it is an int then convert it to a string and add it to the main Astring




-- 4 
--concat2String
-- concat2Str [[AString "enrolled", AString " ", AString "in", AString " "],[AString "CptS", AString "-", AnInt 355], [AString " ", AString "and", AString " "], [AString "CptS", AString "-", AnInt 322]]
 
concat2Str :: [[AnEither]] -> String
concat2Str list = concat (map either_concat list)
     where
          either_concat :: [AnEither] -> String
          either_concat [] = ""
          either_concat list = foldr (\element mainString -> case element of -- case statement to check if the element is of Astring or AInt
                                                            AString string -> string ++ mainString -- if it is a string then add it to the main string
                                                            AnInt intToString -> show intToString ++ mainString) "" list -- if it is an int then convert it to a string and add it to the main string











