import Data.List
import Data.Char
import qualified Data.Map as Map

numUniq :: (Eq a) => [a] -> Int
numUniq = length . nub

numUniq' :: (Eq a) => [a] -> Int
numUniq' xs = length $ nub xs

meh = filter (< 10)

countWords :: String -> [(String, Int)]
countWords = map (\b -> (head b, length b)) . group . sort . words

countWords' :: String -> [(String, Int)]
countWords' = foldr (\b a-> (head b, length b) :a) [] . group . sort . words

countWords'' :: String -> [(String, Int)]
countWords'' xs = map (\b -> (head b, length b)) $ group $ sort $ words xs


isIn :: (Eq a) => [a] -> [a] -> Bool
isIn xs ys = any (isPrefixOf xs) $ tails ys

isIn' :: (Eq a) => [a] -> [a] -> Bool
isIn' xs = any (isPrefixOf xs) . tails

encode :: Int -> String -> String
encode shift msg = map (\v -> chr $ (ord v) + shift) msg

encode' :: Int -> String -> String
encode' shift = map (chr . (+ shift) . ord)

decode :: Int -> String -> String
decode shift = map (chr. (subtract shift) . ord)

decode' :: Int -> String -> String
decode' shift = encode (negate shift)

digitSum :: Int -> Int
digitSum n = foldl (\a b -> a + (digitToInt b)) 0 $ show n

digitSum' :: Int -> Int
digitSum' = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n
  | n <= 0 = Nothing
  | otherwise = find (\x -> digitSum' x == n) [1..]
  
findKey :: (Eq k) => k -> [(k,v)] -> v  
findKey k = snd . head . filter (\(a, b) -> a == k)

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey' k [] = Nothing
findKey' k ((a, b):xs)
  | k ==  a = Just b
  | otherwise = findKey' k xs
  
findKey'' ::   (Eq k) => k -> [(k,v)] -> Maybe v  
findKey'' k = foldr (\(x,y) a -> if (k == x) then Just y else a) Nothing

phoneBook :: Map.Map String String 
phoneBook = Map.fromList [("betty", "555-2938"), 
                          ("bonnie", "452-2928"), 
                          ("patsy", "493-2928"),
                          ("patsy", "493-2920"),                          
                          ("lucille", "205-2929"), 
                          ("wendy", "939-8282"), 
                          ("wendy", "939-8383"),                           
                          ("penny", "853-2492")
                         ]
                         
phoneBook2 :: Map.Map String String 
phoneBook2 = Map.fromListWith (\a b -> a ++ ", " ++ b) [("betty", "555-2938"), 
                         ("bonnie", "452-2928"), 
                         ("patsy", "493-2928"),
                         ("patsy", "493-2920"),                          
                         ("lucille", "205-2929"), 
                         ("wendy", "939-8282"), 
                         ("wendy", "939-8383"),                           
                         ("penny", "853-2492")
                        ]
                        
phoneBook3 =  [("betty", "555-2938"), 
               ("bonnie", "452-2928"), 
               ("patsy", "493-2928"),
               ("patsy", "493-2920"),                          
               ("lucille", "205-2929"), 
               ("wendy", "939-8282"), 
               ("wendy", "939-8383"),                           
               ("penny", "853-2492")
              ]                                                 

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k,a)] -> Map.Map k [a]
phoneBookToMap = Map.fromListWith (++) . map (\(a,b) -> (a, [b]))