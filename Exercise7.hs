-- chapter 7 exercises - higher order functions


--1.  write [f x | x <- xs, p x] as a higher order function (map/filter)

question1 p xs = filter p xs



--2.

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p 

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p  
