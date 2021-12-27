-- chapter 7 exercises - higher order functions


--1.  write [f x | x <- xs, p x] as a higher order function (map/filter)

question1 f xs = filter f xs



--2.

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p 

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p 

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x 	    = x:takeWhile' f xs
					| otherwise = []
					
					
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
  | p x = dropWhile' p xs
  | otherwise = x : xs


--3.       redefine map and filter using foldr

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f =
  foldr
    (\x xs ->
       if f x
         then x : xs
       else xs)
    []


