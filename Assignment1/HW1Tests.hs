{- Example of using the HUnit unit test framework.  See  http://hackage.haskell.org/package/HUnit for additional documentation.
To run the tests type "run" at the Haskell prompt.  -} 

module HW1Tests
    where

import Test.HUnit
import Data.Char
import Data.List (sort)
import HW1
import Data.ByteString.Char8 (null)
import GHC.Float (fromRat'')


-- P1. merge_sorted tests
p1_test5 = TestCase (assertEqual "merge_sorted-test5" 
                                 [1,2,3] 
                                 (merge_sorted [] [1,2,3]) )      

p1_test6 = TestCase (assertEqual "merge_sorted-test6" 
                                 [1,1,2,2,3,3] 
                                 (merge_sorted [1,2,3] [1,2,3]) ) 

p1_test7 = TestCase (assertEqual "merge_sorted-test6" 
                                 [1,1,1,1,1,2,2,2] 
                                 (merge_sorted [1,1,1,1,1] [2,2,2]) )  

 
-- P2. sum_range tests
p2_test4 = TestCase (assertEqual "sum_range-test4" 
                                  0  
                                  (sum_range (7,9)  []) ) 

p2_test5 = TestCase (assertEqual "sum_range-test5" 
                                  7  
                                  (sum_range (7,7)  [0,1,2,3,4,5,6,7]) ) 

p2_test6 = TestCase (assertEqual "sum_range-test6" 
                                  45  
                                  (sum_range (10,5)  [0,1,2,3,4,5,6,7,8,9,10]) ) 


-- P3. (a) calc_collatz_seq and (b) longest_collatz_seq tests                                  


-- P4. (a) game_scores and (b) wins_by_year tests  (one test is sufficient for wins_by_year)                                

                                                           
-- P5. compress_str tests


-- add the test cases you created to the below list. 
tests = TestList [ TestLabel "Problem 1- test5 " p1_test5,
                   TestLabel "Problem 1- test6 " p1_test6,
                   TestLabel "Problem 1- test7 " p1_test7,
                   TestLabel "Problem 2- test4 " p2_test4,
                   TestLabel "Problem 2- test5 " p2_test5,
                   TestLabel "Problem 2- test6 " p2_test6
                 ] 
                  
-- shortcut to run the tests
run = runTestTT  tests