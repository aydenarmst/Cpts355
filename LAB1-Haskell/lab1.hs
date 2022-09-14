-- CptS 355 - Lab 1 (Haskell) - Fall 2022
-- Name: Ayden Armstrong
-- WSU ID: 011672241



module Lab1
     where


-- 1.insert 

insert :: (Ord t1, Num t1) => t1 -> t2 -> [t2] -> [t2]
insert 0 item [] = item:[]
insert n item [] = []
insert n item (x:xs) | (n==0) = (item:x:xs)
                     | otherwise = x:(insert (n - 1) item xs)




-- 2. insertEvery

insertEvery :: (Eq t, Num t) => t -> a -> [a] -> [a]
insertEvery n item list = insertHelper n item list n 
                          where 
                              insertHelper 0 item [] m = item:[]
                              insertHelper n item [] m  = []
                              insertHelper n item (x:xs) m  | (n==0) = (item:x:(insertHelper (m-1) item xs m)) 
                                                            | otherwise = x:(insertHelper (n-1) item xs m)




-- 3. getSales (from zoom lab)

getSales :: (Num p, Eq t) => t -> [(t,p)] -> p
getSales d [] = 0
getSales d ((day,saleData):xs) | (day == d) = saleData + (getSales d xs)  --include sale in the total
                               |  otherwise =  (getSales d xs)  -- exclude the sale in the total




-- 4. sumSales

sumSales :: (Num p) => String -> String -> [ ( String, [(String,p)] ) ] -> p
sumSales companyNameInput dayInput [] = 0
sumSales companyNameInput dayInput ((companyName,saleData):xs) | (companyNameInput == companyName) = (getSales dayInput saleData) + (sumSales companyNameInput dayInput xs )
                                                               | otherwise = sumSales companyNameInput dayInput xs 




-- 5. split

split :: Eq a => a -> [a] -> [[a]]
split chr xs = splitHelper chr xs []
               where
                    splitHelper c [] buf | (buf == []) = []
                                         | otherwise = (reverse buf):[]

                    splitHelper c (x:xs) buf | (x == c) = (reverse buf):(splitHelper c xs [])  -- reset the buffer to []
                                             | otherwise = splitHelper c xs (x:buf)




-- 6. nSplit

nSplit :: (Ord a1, Num a1, Eq a2) => a2 -> a1 -> [a2] -> [[a2]]
nSplit chr numSplits ys = nSplitHelper chr numSplits ys []
nSplitHelper c n [] buf | (buf == []) = []
                        | otherwise = (reverse buf):[] 

nSplitHelper c n (x:xs) buf | (n>0) && (x == c) = (reverse buf):(nSplitHelper c (n-1) xs []) -- n>0, x==c (split) 
                            | (n>0) && (x /= c) = nSplitHelper c n xs (x:buf) -- n>0, x!=c (add x to buf)
                            | otherwise = (x:xs):[]     --- n <= 0 (rest of the list will be the last group)