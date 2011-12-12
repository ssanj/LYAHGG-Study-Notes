lucky :: Int -> String
lucky 7 = "Lucky Number Seven!"
lucky x = "you loose!"

lucky' :: Int -> String
lucky' x = if (x == 7) then "Lucky numero Seven!" else "You loose!"


sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

sayMe' :: Int -> String
sayMe' x = if (x == 1) then "Uno"
	       else if (x == 2) then "Dos"
		   else if (x == 3) then "Tres"
		   else if (x == 4) then "Quattro"
		   else if (x == 5) then "Cinco"
		   else "Not between Uno y Cinco"
		
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1)  (x2, y2) = (x1 + x2, y1 + y2)

first :: (a,b,c) -> a
first (a, _, _) = a

second :: (a,b,c) -> b
second (_,b,_) = b


head' :: [a] -> a
head' [] = error "can't call head on an empty list"
head' (x:_) = x 

head2 :: [a] -> a
head2 xs = if (null xs) then error "can't call head on an empty list" else xs !! 0

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first 2 elements are: " ++ show x ++ " and " ++ show y

tell2 :: (Show a) => [a] -> String
tell2 [] = "The list is empty"
tell2 [x] = "The list has one element " ++ show x
tell2 [x,y] = "The list has 2 elements: " ++ show x ++ " and " ++ show y
tell2 _ = "A big list"


blah :: [a] -> [a] -> [a]
blah xs (a:_) = a : xs

firstLetter :: String -> String
firstLetter "" = "Empty String, Whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> String
bmiTell bmi
 | bmi <= 18.5 = "you are underweight"
 | bmi == 24.5 = "Just right!"
 | bmi <= 25.0 = "you are ugly"
 | bmi <= 30.0 = "you are fat"
 | otherwise = "you are a whale"

bmiTell2 :: Double -> Double -> String
bmiTell2 weight height
 | weight / height ^ 2 <= 18.5 = "you are underweight"
 | weight / height ^ 2 <= 25.0 = "you are ugly"
 | weight / height ^ 2 <= 30.0 = "you are fat"
 | otherwise = "you are a whale"

bmiTell3 :: Double -> Double -> String
bmiTell3 weight height
 | bmi <= 18.5 = "you are underweight"
 | bmi <= 25.0 = "you are ugly"
 | bmi <= 30.0 = "you are fat"
 | otherwise = "you are a whale"
 where bmi = weight / height ^ 2

bmiTell4 :: Double -> Double -> String
bmiTell4 weight height
 | bmi <= skinny = "you are underweight"
 | bmi <= ugly = "you are ugly"
 | bmi <= fat = "you are fat"
 | otherwise = "you are a whale"
 where bmi = weight / height ^ 2
       skinny = 18.5
       ugly = 25.0
       fat = 30.0

bmiTell5 :: Double -> Double -> String
bmiTell5 weight height
 | bmi <= skinny = "you are underweight"
 | bmi <= ugly = "you are ugly"
 | bmi <= fat = "you are fat"
 | otherwise = "you are a whale"
 where bmi = weight / height ^ 2
       (skinny, ugly, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
 | a <= b = b
 | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a == b = EQ
  | a < b = LT
  | otherwise = GT

niceGreeting = "Hello! Very nice to see you, "
badGreeting = "Oh! Pfft. It's you, "

greet :: String -> String
greet name
  | name == "Juan" || name == "Fernando" = niceGreeting ++ name
greet name = badGreeting ++ name       

initials :: String -> String -> String
initials first last = [f] ++ "." ++ [l] ++ "."
  where (f:_) = first
        (l:_) = last 

initials2 :: String -> String -> String
initials2 [] _ = "Supply a first and last name"
initials2 _ [] = "Supply a first and last name"
initials2 (f:_) (l:_) = [f] ++ "." ++ [l] ++ "."

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [ w `bmi` h | (w,h) <- xs]
  where bmi w h = w / h ^ 2

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2*pi*r*h
        topArea = pi*r ^ 2
        myVar x = x + 1.0
    in  sideArea + 2 * topArea + (myVar r)


calcBmis2 :: [(Double, Double)] -> [Double]
calcBmis2 xs = [ doubleMe bmi | (w,h) <- xs, let bmi = w / h ^ 2;doubleMe x = x + x, bmi > 25.0]

head3 :: [a] -> a
head3 xs = case xs of [] -> error "No head for empty lists"
                      (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "a singleton list"
                                               (_:_) -> "a longer list"	
describeList2 :: [a] -> String
describeList2 xs = "The list is " ++ what xs
  where what [] = "empty"
        what [x] = "a singleton list"
        what xs = "a longer list"

{-- 
 Question 1 
 Define a recursive function mult that takes two positive integers a and b and 
 returns a * b, but only uses addition.
--}

mult :: Int -> Int -> Int
mult 0 _ = 0
mult _ 0 = 0
mult a b = a + (a `mult` (b - 1))


{-- 
 Question 2
 Rewrite the following function using let statements

roots a b c =
    ((-b + sqrt(b*b - 4*a*c)) / (2*a),
     (-b - sqrt(b*b - 4*a*c)) / (2*a))
--}

roots1 a b c = 
	let f1 = sqrt(b*b - 4*a*c)
	    f2 = 2*a
	in ((-b + f1) / f2, (-b - f1)/f2)
	
roots2 a b c = ((-b + f1) / f2, (-b - f1)/f2)	
  where f1 = sqrt(b*b - 4*a*c) 
        f2 = 2*a	
        
{-- 
Question 3
Using Guards, define a function named sign that takes a single numeric parameter.
The function will return 1 if the parameter is positive, -1 if the parameter is negative 
or 0 otherwise.
--}        
sign :: (Ord a, Num a) => a -> Int
sign n
  | n < 0 = (-1)
  | n > 0 = 1
  | otherwise = 0
  
{-- 
Question 4
 Euler Problem 1:
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000. (234168)

Solve Euler Problem 1 using each of the following language constructs:
    a) Pattern Matching
    b) Guards
    c) Where
    d) Let
    e) Case
--}

euler1pm :: Int -> Int
euler1pm 1000 = sum [3,6..999] + sum [5,10..999] - sum [15,30..999]
euler1pm _ = error "only calculates for a thousand"  

euler1grd :: Int -> Int
euler1grd x
  | x  == 1000 = sum [3,6..999] + sum [5,10..999] - sum [15,30..999]
  | otherwise = error "only calculates for a thousand"
  
{-- Totally unexpected! --}  
euler1grd2 :: Int -> [Int]
euler1grd2 n = [x | x <- [1 .. (n-1)], (x `mod` 3 == 0 || x `mod` 5 == 0)]
  
  
euler1whr :: Int -> Int
euler1whr 1000 = calcSum
    where calcSum = sum [3,6..999] + sum [5,10..999] - sum [15,30..999]
euler1whr _ = error "only calculates for a thousand"

euler1let :: Int -> Int
euler1let 1000 = let calcSum = sum [3,6..999] + sum [5,10..999] - sum [15,30..999] in calcSum
euler1let _ = error "only calculates for a thousand"

euler1cse :: Int -> Int
euler1cse x = case x of 999 -> sum [3,6..999] + sum [5,10..999] - sum [15,30..999]
                        _ -> error "only calculates for a thousand"
                        
roots3 a b c =
  let f1 = 4 * a * c
      f2 = 2 * a                        
      f3 = sqrt (b*b - f1)
  in ((-b + f3)  / f2, (-b - f3) / f2)
  
  
chooseName :: String -> String
chooseName n = getRandomName n
  where getRandomName :: String -> String
        getRandomName "sanj" = myGreet n
          where myGreet g = "Hello " ++ g ++ (let adjective = " You rock" in adjective ++ " the spot")
        getRandomName "blah" = "Howdy Blah"    
        getRandomName x = "Pleased to meet you"