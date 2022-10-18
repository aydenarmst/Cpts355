-- CptS 355 - Fall 2022 -- Homework2 - Haskell
-- Name: Ayden Armstrong 
-- Collaborators: 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}


module HW2
     where

{- P1 - insert, insert_tail -}

-- (a) insert – 3%
insert :: (Ord t1, Num t1) => t1 -> t2 -> [t2] -> [t2]
insert 0 item [] = [item]
insert _ item [] = []
insert n item (x:xs) | (n==0) = (item:x:xs)
                     | otherwise = x:(insert (n - 1) item xs)

-- (b) insert_tail –  10%
insert_tail :: (Ord t1, Num t1) => t1 -> t2 -> [t2] -> [t2]
insert_tail n item [] = [] -- base case, if the list is empty, return an empty list
insert_tail n item (x:xs) = insert_tail_helper n item (x:xs) [] -- helper function to keep track of the tail with an accumulator
                         where
                               insert_tail_helper 0 item [] acc = acc ++ [item] -- if the index is 0, and the list is empty, add the item to the end of the accumulator
                               insert_tail_helper _ item [] acc = acc -- base case, if the index is not 0, and the list is empty, return the accumulator
                               insert_tail_helper n item (x:xs) acc | (n==0) = acc ++ (item:x:xs) -- insert item at n is 0
                                                                    | otherwise = insert_tail_helper (n - 1) item xs (acc ++ [x]) -- otherwise, keep going through the list and append to the accumulator

------------------------------------------------------
{- P2  game_scores and wins_by_year  -}

-- (a) game_scores – 12%
game_scores :: Eq t => [(a1, [(t,a2)])] -> t -> [a2]
game_scores [] _ = [] -- base case, if the list is empty, return an empty list
game_scores log findteam = map snd (filter (checkTuple findteam) (concat (map snd log)) ) -- map the seconds of the team to the list of tuples, then filter the list of tuples to only include the team we are looking for with the helper. Finally concat the list of tuples to a list of tuples of the scores we are looking for. Taken from concat all in lab2
                          where
                               checkTuple findteam (team, _) = (findteam == team) -- if the team is in the tuple, return true


-- (b) wins_by_year – 12%
wins_by_year :: (Num b, Ord a1) => [(a2, [(a3, (a1,a1))])] -> [(a2,b)]
wins_by_year [] = [] -- base case, if the list is empty, return an empty list
wins_by_year log = map (wins_by_year_helper) log -- map the helper function to the list of tuples
                 where
                      wins_by_year_helper (year, teams) = (year, sum (map (checkTuple) teams)) -- fold the list of tuples to a single number which is 1 if wsu won and 0 otherwise for each game, then map the helper function to the rest of the other game tuples for that year

                      checkTuple (_, (wsu, opposingTeam)) | (wsu > opposingTeam) = 1 -- if wsu won, return 1
                                                          | otherwise = 0 -- otherwise, return 0

------------------------------------------------------
{- P3. sum_nested_int, sum_nested_item, and sum_my_nested -}

data NestedList = Item Int
                  | Array [Int]
                  deriving (Show, Read, Eq)


-- (a) sum_nested_int - 8%
sum_nested_int :: [NestedList] -> Int
sum_nested_int [] = 0 -- base case, if the list is empty, return 0
sum_nested_int (x:xs) = sum (map (sum_nested_int_helper) (x:xs)) -- map the helper function to the list of nested lists, then sum the list of integers
                        where
                                  sum_nested_int_helper (Item x) = x -- if the item is an integer, return the integer
                                  sum_nested_int_helper (Array x) = sum x -- if the item is an array, sum the array



-- (b) sum_nested_item - 5% 
sum_nested_item :: [NestedList] -> NestedList
sum_nested_item [] = Item 0 -- base case, if the list is empty, return 0
sum_nested_item (x:xs) = Item (sum (map (sum_nested_item_helper) (x:xs))) -- map the helper function to the list of nested lists, then sum the list of integers to an Item value type.
                          where
                                  sum_nested_item_helper (Item x) = x -- if the item is an integer, return the integer
                                  sum_nested_item_helper (Array x) = sum x -- if the item is an array, sum the array


{- new datatype for part 3 -}
data MyNested = MyItem Int
              | MyArray [MyNested]
              deriving (Show, Read, Eq)


-- (c) sum_my_nested - 12% 
sum_my_nested :: [MyNested] -> Int
sum_my_nested [] = 0 -- base case, if the list is empty, return 0
sum_my_nested (x:xs) = sum (map (sum_my_nested_helper) (x:xs)) -- map the helper function to the list of nested lists, then sum the list of integers
                        where
                                  sum_my_nested_helper (MyItem x) = x -- if the item is an integer, return the integer
                                  sum_my_nested_helper (MyArray x) = sum_my_nested x -- if the item is an array, recursively call the function on the array


------------------------------------------------------
{- P4 tree_height, create_htree, tree_paths  -}

data Tree a = Leaf a | Node a (Tree a) (Tree a)
             deriving (Show, Read, Eq)


data HTree a = HLeaf a | HNode a Int (HTree a) (HTree a)
              deriving (Show, Read, Eq)


--  (a) tree_height - 8% 
tree_height :: Tree a -> Int
tree_height (Leaf _) = 1 -- base case, if the tree == leaf return 1
tree_height (Node _ left right) = 1 + max (tree_height left) (tree_height right) -- otherwise, recursively call the function on the left and right subtrees, then add 1 to the max of the two heights


-- (b) create_htree - 10%
create_htree :: Tree a -> HTree a
create_htree (Leaf x) = HLeaf x -- base case, if the tree == leaf return the leaf without the height value
create_htree (Node x left right) = HNode x (1 + max (tree_height left) (tree_height right)) (create_htree left) (create_htree right) -- otherwise, recursively call the function on the left and right subtrees, then add 1 to the max of the two heights, then create a new HTree with the height


-- (c) find_paths - 13%
find_paths :: Eq a => HTree a -> a -> [[a]]
find_paths (HLeaf x) v | (x == v) = [[x]] 
                       | otherwise = [] -- base case, if the tree == leaf and the value is equal to the value we are looking for, return the leaf, otherwise return an empty list
find_paths (HNode x _ left right) v | (x == v) = [] -- if the node value is equal to the value we are looking for return an empty list
                                    | otherwise = map (x:) (find_paths left v) ++ map (x:) (find_paths right v) -- otherwise, recursively call the function on the left and right subtrees, then map the node value to the list of paths from the left and right subtrees, then concat the two lists together


{- Tree Examples -}
testTree1 = Node 1 (Node 2 (Node 3 (Leaf 4) (Leaf 5)) (Node 6 (Leaf 7) (Leaf 8))) (Node 9 (Node 10 (Leaf 11) (Leaf 12)) (Node 13 (Leaf 14) (Leaf 15)))

testTree2 = Node 'A' (Node 'B' (Node 'C' (Leaf 'D') (Leaf 'E')) (Node 'F' (Leaf 'G') (Leaf 'H'))) (Node 'I' (Node 'J' (Leaf 'K') (Leaf 'L')) (Node 'M' (Leaf 'N') (Leaf 'O')))