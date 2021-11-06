-- chapter 6 exercises - Recursion

--1. defining the operation (^) 

expo :: (Integral a) => a -> a -> a
expo _ 0 = 1
expo a b
  | b > 0 = a * (expo a (b - 1))
  
  
  
--2.

init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x: init' xs

-- init [1,4,2,5]
-- 1: init [4,2,5]
-- 1:4: init [2,5]
-- 1:4:2: init [5]
-- 1:4:2:[]
-- [1,4,2]


length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- length [1,3,4,5]
-- 1 + length [3,4,5]
-- 1 + ( 1 + length [4,5])
-- 1 + ( 1 + ( 1 + length [5]))
-- 1 + ( 1 + ( 1 + ( 1 + length [])))
-- 1 + ( 1 + ( 1 + ( 1 + 0)))
-- 4






--3. 

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && (and' xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' [x] = x
concat' (x:xs) = x ++ (concat' xs)


replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = [x] ++ replicate' (n-1) x 


select' :: [a] -> Int -> a
select' (x:xs) 0 = x 
select' (x:xs) n = select' xs (n-1)


elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False 
elem' n (x:xs)| n == x    = True
			  | otherwise = elem' n xs
			  
			  

--4.

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) = if x < y
                        then x:(merge xs (y:ys))
                        else y:(merge (x:xs) ys)
merge [] xs = xs
merge xs [] = xs 

--merge [2,5,6] [1,3,4]
--1:(merge ([2,5,6]) [3,4])
--1:(2:(merge [5,6] [3,4])
--1:(2:(3: merge [5,6] [4])
--1:(2:(3:(4:( merge [5,6] [])
--1:(2:(3:(4:([5,6])
--[1,2,3,4,5,6]




--5.

halve :: [a] -> ([a],[a])
halve xs = ((take n xs),(drop n xs))
		   where n = length xs `div` 2
		   

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort  xs = merge (msort left) (msort right)
            where (left,right) = halve xs
		   	



--6.

--calculate sum of a list
listSum :: [Int] -> Int
listSum [] = 0
listSum (x:xs) = x + (listSum xs) 

--take given number of elements from start of a list
takeList :: Int -> [a] -> [a]
takeList n [] = []
takeList 0 xs = xs 
takeList n (x:xs) = takeList (n-1) xs 

--select last element from a non empty list
selectLast :: [a] -> a
selectLast [x] = x
selectLast (x:xs) = selectLast xs 




