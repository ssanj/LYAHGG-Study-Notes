removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addTwoToFive :: Int -> Int -> Int
addTwoToFive = addThree 5

plus :: Int -> Int -> Int
plus a b = a + b

plusTwo :: Int -> Int
plusTwo = plus 2

plusOne :: Int -> Int
plusOne a = plus 1 a

inc' :: Int -> Int
inc' a = plus 1 a

inc'' :: Int -> Int
inc'' = plusOne

morePlus :: Int -> (Int -> Int)
morePlus x y = x + y

factorial :: Int -> Int
factorial n = product [1..n]

factorial' :: Integer -> Integer
factorial' n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

{-- Question 1 --}
radius :: Integer -> Double
radius r = 2 * pi * (fromIntegral r)


{-- Question 2 --}
push :: (Num a) => a -> [a] -> [a]
push x xs = x : xs

{-- Question 3--}
(->>) :: (Num a) => a -> [a] -> [a]
(->>) = push 

{-- Does not work with Real numbers. --}
(>>>) = push