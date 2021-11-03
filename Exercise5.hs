--Chapter 5 exercise


import Data.List

--1. List comprehensions to calculate the sum of 1^2 + 2^2 + 3^2 ... 100^2

sumSquaredToHundred = sum [x^2 | x <- [1..100]]



--2. re defining the library function replicate using list comprehensions

replicate' x y = [ y | _ <- [1..x]] 



--3. find all pythagorean triples up to a limit

pythagTrip a = [(x,y,z) | x <- [1..a], y <- [1..a],z <- [1..a], x^2 + y^2 == z^2]



--4. find list of perfect integers up until a given limit

factors :: Int -> [Int]
factors n = [x|x <- [1..n], n `mod` x == 0, n/=x]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x]




--5. show how [(x,y) | x <- [1,2,3], y <- [4,5,6]] can be redifined using two 
	 --comprehensions with a single generator each 
	 
questionFive = concat [ [(x,y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]





--6. redifine the function 'positions' using the function find

find' :: Eq a => a -> [(a,b)] -> [b]
find' x xs = [ i | (x', i) <- xs, x == x' ]

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = [ i | i <- find' x (zip xs [0..n])]
                 where n = length xs - 1
				 
				 
				 

--7. return scaler produc of two lists

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [ x*y | (x,y) <- zip xs ys]



 











