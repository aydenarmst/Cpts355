-- CptS 355 - Fall 2022 -- Homework1 - Haskell
-- Name: Ayden Armstrong
-- Collaborators: 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}





module HW1
     where
import Data.Char (isDigit)



-- P1 - merge_sorted 10%
merge_sorted :: Ord a => [a] -> [a] -> [a]
merge_sorted [] xs = xs
merge_sorted xs [] = xs
merge_sorted (x:xs) (y:ys)  = if x < y then x : merge_sorted xs (y:ys)
                              else  y : merge_sorted (x:xs) ys



-- P2  sum_range  15%
sum_range :: (Ord a, Num p, Num a) => (a, a) -> [p] -> p
sum_range range list = sum_helper range list 0
                       where
                         sum_helper _ [] _ = 0  -- empty list returns 0 
                         sum_helper (start,finish) (x:xs) counter | counter == finish = x
                                                                  | start > finish = x + sum_helper(finish,start) xs (counter + 1)  -- if they put the start greater than finish for some reason than it will calculate the range between them still :) 
                                                                  | counter >= start = x + sum_helper(start,finish) xs (counter + 1)
                                                                  | otherwise = sum_helper (start,finish) xs (counter + 1)

-- P3  (a) calc_collatz_seq ; 10%

calc_collatz_seq :: Integral a => a -> [a]
calc_collatz_seq num | num == 1 = [1]
                     | even num = num : calc_collatz_seq (num `div` 2)
                     | otherwise = num : calc_collatz_seq (3 * num + 1)





-- P3  (b) longest_collatz_seq ; 15%

longest_collatz_seq :: Integral a => a -> [a]
longest_collatz_seq num = longest_helper num num 1
                          where
                            longest_helper max end counter | length(calc_collatz_seq max) >= length(calc_collatz_seq counter) = if counter == end then calc_collatz_seq max
                                                                                                                                else longest_helper max end (counter + 1)
                                                           | otherwise = if counter == end then calc_collatz_seq counter
                                                                         else longest_helper counter end (counter + 1)



-- P4  (a) game_scores ; 15%
-- (2020, [("ORST",(38,28)), ("ORE",(29,43)), ("USC",(13,38)), ("UTAH",(28,45))]),
-- (2021, [("USU",(23,26)), ("PORT ST.",(44,24)), ("USC",(14,45)), ("UTAH",(13,24)), ("CAL",(21,6)), ("ORST",(31,24)), ("STAN",(34,31)), ("BYU",(19,21)),("ASU",(34,21)), ("ORE",(24,38)), ("ARIZ",(44,18)), ("WASH",(40,13)), ("CMU",(21,24))] )]

game_scores :: Eq t => [(a1, [(t,a2)])] -> t -> [a2]
game_scores [] _ = []
game_scores (x:xs) school = game_helper x school ++ game_scores xs school   -- using lazy evaluation of appedning the list, i tried to add this to the end of the then statement, but it wouldnt add the scores if they werent in the first list, maybe you can help me understand 
                            where
                                game_helper (_,[]) _ = []
                                game_helper (const, y:ys) school = if fst y == school then snd y : game_helper (const, ys) school
                                                                   else game_helper (const, ys) school



-- P4  (b) wins_by_year ; 10%
wins_by_year :: (Num b, Ord a1) => [(a2, [(a3, (a1,a1))])] -> [(a2,b)]
wins_by_year [(year, [])]






-- P5  compress_str ; 15% 
--compress_str :: [Char] -> [Char]
--compress_str [] = []



-- Assignment rules ; 4%
-- Your own tests ; 6%
