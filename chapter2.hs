import Data.Char

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k xs = [ b | (a,b) <- xs, k == a]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

pairs' :: Num a => [a] -> [(a,a)]
pairs' xs = [ (x,(x + 1)) | x <- xs, last xs /= x]

count :: Char -> String -> Int
count c xs = length [ 1 | x <- xs, x == c]

lowers :: String -> Int
lowers xs = length [ 1 | x <- xs, isLower x]

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

sortedWith :: Ord a => ((a,a) -> Bool) -> [a] -> Bool
sortedWith f xs = and [f(x,y) | (x,y) <- pairs xs]

sortedWith' :: Ord a => (a -> a -> Bool) -> [a] -> Bool
sortedWith' f xs = and [f x y | (x,y) <- pairs xs]

pos :: Eq a => a -> [a] -> [Int]
pos n xs = [p | (p, v)<- (zip [0..] xs), v == n]

pos' :: Eq a => a -> [a] -> [Int]
pos' a xs = find a (zip xs [0..])

hasLetters :: [Char] -> Bool
hasLetters xs = length ([c | c <- xs, c `elem` ['A'..'z']]) > 0 

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [ shift n x | x <- xs]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m)  * 100

freq :: String -> [(Char, Float)]
freq xs = [ (c, (percent (count c xs) n)) | c <- ['a'..'z']]
          where n = lowers xs

rotate :: Int -> String -> String
rotate n xs = drop n xs ++ take n xs

sqSum :: Int -> Int
sqSum n = sum [x*x | x <- [1..n]]

replicate' :: Int -> a -> [a]
replicate' n a = [a | _ <- [1..n]]
