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




