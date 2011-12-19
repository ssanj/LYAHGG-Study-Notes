fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "can't call maximum' on an empty list!"
maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)
  where max' :: (Ord a) => a -> a -> a
        max' x y
          | x >= y = x
          | otherwise = y
          
          
replicate' :: Int -> a -> [a]
replicate' n v
 | n <= 0 = []
 | otherwise = v : replicate' (n-1) v
 
take' :: Int -> [a] -> [a]
take' n xs
  | n <= 0 = []
  | null xs = []
take' n (x:xs) = x : take' (n-1) xs 


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

cycle' :: [a] -> [a]
cycle' xs = xs ++ cycle' xs 

zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys
  | null xs = []
  | null ys = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys  

elem' :: (Eq a) => a -> [a] -> Bool
_ `elem'` [] = False
v `elem'` (x:xs)
  | v == x = True
  | otherwise = v `elem'` xs
  
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      largerThan = [a | a <- xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort largerThan    
  
{-- 
Question 1
The double factorial of a number n is the product of every other number from 1 (or 2) up to n.

For example, the double factorial of 8 is: 8 × 6 × 4 × 2 = 384, and the double factorial of 7 is: 7 × 5 × 3 × 1 = 105.
--}  

doubleFactorial :: Int -> Int
doubleFactorial n
  | n <= 1 = 1
doubleFactorial n = n * doubleFactorial (n - 2)

doubleFactorial' :: Int -> Int -> Int
doubleFactorial' n t
  | n <= 1 = t
doubleFactorial' n t = doubleFactorial' (n - 2)  n * t

doubleFactorial'' :: Int -> Int
doubleFactorial'' n
 | n <= 1 = 0
doubleFactorial'' n = doDoubleFactorial n 1
  where doDoubleFactorial :: Int -> Int -> Int
        doDoubleFactorial n t
         | n <= 1 = t
         | otherwise = doDoubleFactorial (n - 2) n * t

{-- 
Question 2

Implement the function log2, which computes the integer log (base2) of its argument. That is, log2 computes the exponent of the largest power of 2 which is less than or equal to its argument.

For example, log2 16 = 4, log2 11 = 3, and log2 1 = 0.
--}

log2 :: Integer -> Integer
log2 n
  | n <= 1 = 0
  | otherwise = 1 + log2 (n `div` 2)
  
  
log2' :: Integer -> Integer -> Integer
log2' n t
  | n <= 1 = t
  | otherwise = log2' (n `div` 2) 1+t
  
log2'' :: Integer -> Integer
log2'' n
  | n <= 1 = 0
log2'' n =
  let doLog2 n t 
       | n <= 1 = t
      doLog2 n t = doLog2 (n `div` 2) 1 + t 
  in doLog2 n 0

{-- 
Question 3

Write a recursive function to solve the classic Towers of Hanoi problem. 
Call it hanoi n, where n is the number of rings you need to move from the 
first post to the third post in the puzzle.

The game consists of three rods, and a number of disks of different sizes 
which can slide onto any rod. The puzzle starts with the disks in a neat 
stack in ascending order of size on one rod, the smallest at the top, 
thus making a conical shape. The objective of the puzzle is to move the 
entire stack to another rod, obeying the following rules:

1. Only one disk may be moved at a time.
2. Each move consists of taking the upper disk from one of the rods and sliding it 
   onto another rod on top of the other disks that may already be present on that rod.
3. No disk may be placed on top of a smaller disk.
--}

hanoi :: a -> a -> a -> Int -> [(a, a)]
hanoi source using dest n
    | n == 0 = []
    | n == 1 = [(source, dest)]
    | otherwise = hanoi source dest using (n-1) 
                  ++ hanoi source using dest 1
                         ++ hanoi using source dest (n-1)
                         
-- reference -> http://www.haskell.org/haskellwiki/Common_Misunderstandings)                         

hanoi' :: Int -> [[[Int]]]
hanoi' n
  | n <= 0 = []
hanoi' n = let initBoard = [[[1..n], [], []]]
               move :: [[[Int]]] -> [[[Int]]]
               move x@[[[1], [], []]] = x ++ [[[], [], [1]]]
               move x@[[[1,2], [], []]] = move (x ++ [[[2], [1], []]])
               move x@[[[1,2], [], []], [[2], [1], []]] = move (x ++ [[[], [1], [2]]])
               move x@[[[1,2], [], []], [[2], [1], []], [[] , [1], [2]]] = x ++ [[[],[],[1,2]]]             
          in move initBoard    