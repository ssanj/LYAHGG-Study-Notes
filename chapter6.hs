add :: Int -> Int -> Int
add x y = x + y

threed ::  Int -> Int -> Int -> (Int, Int, Int)
threed x y z = (x, y, z)

multThree :: Int -> (Int -> (Int -> Int))
multThree x y z =  x * y * z

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

addStrLen :: String -> Int -> Int
addStrLen s n = (length s) + n

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs
  
largestDiv :: Integer
largestDiv = last [x | x <- [1..100000], x `mod` 3829 == 0]  

largestDiv' :: Integer
largestDiv' = head [x | x <- [100000, 99999 .. 0], x `mod` 3829 == 0]  

largestDiv'' :: Integer
largestDiv'' = head (filter p [100000, 99999 .. 0])
  where p x = x `mod` 3829 == 0

largestDiv''' :: Integer
largestDiv''' = head (filter (\x -> x `mod` 3829 == 0) [100000, 99999 .. 0])

sumOfOddSq :: Integer
sumOfOddSq =  sum (takeWhile (< 10000) [x^2 | x <-[1..], odd (x^2)])

sumOfOddSq' :: Integer
sumOfOddSq' =  sum (takeWhile (< 10000)  (filter odd (map (^2) [1..])))

sumOfOddSq'' :: Integer
sumOfOddSq'' =  sum (takeWhile (< 10000) [sq x | x <-[1..], odd (sq x)])
  where sq x = x ^ 2

sumOfOddSq''' :: Integer
sumOfOddSq''' =  sum (takeWhile (< 10000) [ y | y <- [x^2 | x <-[1..]], odd y])


collatz :: Integer -> [Integer]
collatz n
 | n <= 0 = []
 | n == 1 = [1]
 | even n = n : collatz (n `div` 2)
 | odd n = n : collatz (n * 3 + 1)
 
numLongChains :: Int -> Int
numLongChains n =  length (filter (\xs -> length xs > n) [collatz x | x <- [1..100]])

numLongChains' :: Int -> Int
numLongChains' n =  length (filter isLong (map collatz [1..100]))
 where isLong x = length x > 15
 
addThree :: Int -> Int -> Int -> Int
addThree = \x -> \y -> \z -> x + y + z

addThree' :: Int -> Int -> Int -> Int
addThree' = \x y z -> x + y + z

addThree'' :: Int -> Int -> Int -> Int
addThree'' x y z = x + y + z

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\a b -> a + b) 0 xs


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Can't call maximum' on an empty list"
maximum' (x:xs) = foldl (max) x xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x a -> f x : a) [] xs

mapr :: (a -> b) -> [a] -> [b]
mapr f  = foldr (\x a -> f x : a) []

mapl :: (a -> b) -> [a] -> [b]
mapl f = foldl (\a x -> a ++ [f x]) []

elem' :: (Eq a) => a -> [a] -> Bool
elem' i xs = foldr (\x a -> i == x || a) False xs

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldl1 max 

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\v a -> if (f v) then v : a else a) []

last' :: [a] -> a
last' [] = error "last on an empty list"
last' xs = head (foldr (\v a -> if null a then v : a else a) [] xs)

last'' :: [a] -> a
last'' =  foldl1 (\_ v -> v)

and' :: [Bool] -> Bool
and' = foldr (&&) True

or' :: [Bool] -> Bool
or' = foldr (||) False

or'' :: [Bool] -> Bool
or'' = foldr (||) False

sqrtUnder1000 :: Integer
sqrtUnder1000 =  toInteger (length (takeWhile (< 1000) (scanr1 (+)  (map (sqrt) [1..]))) + 1)

dec2nat :: [Int] -> Int
dec2nat [] = 0
dec2nat xs = read (foldl (\a v -> a ++ show v) "" xs)

fivePlusEightBy x = (8/x) + 5

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \x -> \y -> f (x,y)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (x, y) = f x y

mapWithFilter :: (a-> b) -> (a->Bool) -> [a] -> [b]
mapWithFilter f p xs = map f (filter p xs)

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' :: Integer -> Integer
fib' n
 | n == 0 = 0
 | n == 1 = 1
fib' n =  last $ scanl (\a b -> fib' (b - 1) + fib' (b - 2)) 0 [2..n]

length' :: [a] -> Integer
length' = foldl (\a _ -> a + 1) 0

append' :: [a] -> [a] -> [a]
append' xs ys = foldr (:) ys xs

flatten' :: [[a]] -> [a]
flatten' = foldr (\b a -> foldr (:) a b) []

flatten'' :: [[a]] -> [a]
flatten'' xxs = [x | xs <- xxs, x <- xs]


flatMap' :: (a -> [b]) -> [a] -> [b]
flatMap' f xs = flatten' $ map f xs


fn x = ceiling (negate (tan (cos (max 50 x))))
fn' = ceiling . negate . tan . cos . max 50
-- fn'' = ceiling $ negate $ tan $ cos $ max 50

{-- 
curry f = \x y -> f (x, y)
uncurry f = \(x, y) -> f x y
--}

fst' :: (a,b) -> a
fst' = uncurry const

snd' :: (a, b) -> b
snd' = uncurry $ flip const

testProb1Curry = (curry' fst) 11 22 == 11
testProb1Uncurry = (uncurry' (+)) (12,23) == 35
testProb2  = mapWithFilter (+11) odd [1..10] == [12,14,16,18,20]
testProb3Length  = length' [1..10] == 10
testProb3Append  = append' "has" "kell" == "haskell"
testProb3Flatten = flatten' ["abc","def","ghi"] == "abcdefghi"
testProb3Flatmap = flatMap' (replicate 3) [1..4] == [1,1,1,2,2,2,3,3,3,4,4,4]
testProb4 = dec2nat [2, 3, 4, 5] == 2345
testProb5 = fib' 10 == 55
testProb6 = fivePlusEightBy 2 == 9
testProb7Fst = fst' (1, 2) == 1
testProb7Snd = snd' (1, 2) == 2