-- TYPES AND CLASSES

--1.

data Nat = Zero | Succ Nat
	deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)


mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = add n (mult m n)


--3.

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

ex1 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3)(Leaf 4))

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = abs (size l - size r) <= 1 && balanced l && balanced r

size :: Tree a -> Int
size (Leaf _) = 1
size (Node l r) = size l + size r


--4.

listSplitter :: [a] -> ([a],[a])
listSplitter xs = splitAt ((length xs) `div` 2) xs 

balance :: [a] -> Tree a 
balance [] = error "Empty list"
balance [x] = Leaf x
balance xs = Node (balance l) (balance r)
  where
    (l, r) = halve xs











