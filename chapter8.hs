import qualified Data.Map as M
data Point = Point Float Float deriving Show
data Shape = Circle Point Float | Rectangle Point Point deriving Show

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRectangle :: Float -> Float -> Shape
baseRectangle w h = Rectangle (Point 0 0) (Point w h)

{--
data Person = Person String String Int Float String String deriving (Show)

firstName :: Person -> String
firstName (Person name _  _  _  _  _) = name

lastName :: Person -> String
lastName (Person _ last _ _ _ _) = last
--}

-- interferes with the automatically generated getters on Person.
--blah :: Person -> String
--blah (Person _ _ _ _ _ _ c) = c

data Person = Person { firstName :: String,
                       lastName :: String,
                       age :: Int,
                       height :: Float,
                       phoneNumber :: String,
                       flavor :: String,
                       blah :: String } deriving (Show)
                       
data Box a = Empty' | Full a deriving Show

data Vector a = Vector a a a deriving Show

vplus :: (Num a) => Vector a -> Vector a -> Vector a                       
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `dotProd` (Vector l m n) = Vector (i*l) (j*m) (k*n)

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

vzipWith :: (Num a) => (a -> a -> a) -> Vector a -> Vector a -> Vector a 
vzipWith f (Vector i j k) (Vector l m n) = Vector (i `f` l) (j `f` m) (k `f` n)

data Person2 = Person2 { fname :: String, lname :: String } deriving (Show, Eq, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Show, Read, Ord, Enum, Bounded)


type PrettyString = String
type PhoneBook = [(Name, PhoneNumber)]
type Name = String
type PhoneNumber = String

phoneBook :: PhoneBook
phoneBook = [("betty", "5555-2938"), ("penny", "853-2492")]

inPhoneBook :: Name -> PhoneBook -> Bool
inPhoneBook name pb = (filter (\(n, _) -> n == name) pb) /= []

type IntMap v = M.Map Int v

myIntMap :: IntMap String
myIntMap = M.fromList [(1, "one"), (2, "two")]


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = M.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup n map = case M.lookup n map of
      Nothing -> Left $ "Locker" ++ show n ++ " doesn't exist!"
      Just (state, code) -> if state /= Taken then Right code else Left $ "Lcoker " ++ show n ++ " is already taken!"
      
lockers :: LockerMap
lockers = M.fromList[(100, (Taken, "HNDRD")),(200, (Free, "TWOHNDR")),(201, (Taken, "TWOONE"))]      

infixr 5 :-:
data List a = Empty |  a :-: (List a)  deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)
  
  
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)  
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

data TrafficLight = Red | Yellow | Green deriving (Enum, Bounded)

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False


instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"


class YesNo a where
  yesno :: a -> Bool
  
  
instance YesNo Int where
  yesno n 
    | n <= 0 = False
    | otherwise = True
    
-- can't use [] as the type parameter to YesNo as List is a type constructor not a type.    
instance YesNo [a] where
  yesno [] = False
  yesno _ = True  

instance YesNo Bool where
  yesno = id
  
instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False  
  
instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno (Node _ _ _) = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True  


yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal x y = if yesno yesnoVal then x else y

-- Question 3

data Currency = Currency { name :: String, value :: Float, amount :: Int }  deriving (Show, Read, Eq, Ord)

data CurrencySupply a = NoMoney | SomeMoney (M.Map a Currency)

type Stash = CurrencySupply

instance (Show a) => Show (CurrencySupply a) where
   show NoMoney = "No Money! :( "
   show (SomeMoney a) = "Money! :) -> " ++ show a
   
instance (Eq a) => Eq (CurrencySupply a) where
  NoMoney == NoMoney = True
  SomeMoney (m1) == SomeMoney (m2) = m1 == m2
  _ == _ = False

currencyList :: M.Map String Currency
currencyList = M.fromListWith (\(Currency {name = _, value = _, amount = a1}) (Currency {name = n, value = v, amount = a2}) -> Currency {name = n, value = v, amount = (a1 + a2) }) 
  [("AUD20", (Currency { name = "AUD", value = 20, amount = 2 })), 
   ("AUD10", (Currency { name = "AUD", value = 10.0, amount = 15 })), 
   ("GBP10", (Currency { name = "GBP", value = 10.0, amount = 2 })),   
   ("SLRP", (Currency { name = "SLRP", value = 100.0, amount = 2000 })),      
   ("RGT", (Currency { name = "RGT", value = 20.0, amount = 16 })),         
   ("AUD20", (Currency { name = "AUD", value = 20, amount = 3 }))]

aSupply = SomeMoney currencyList

lookupSupplyFor :: (Ord a) => (Currency -> Maybe b) -> a -> CurrencySupply a -> Maybe b
lookupSupplyFor _ _ NoMoney = Nothing
lookupSupplyFor f key (SomeMoney (m)) = case M.lookup key m of 
  Just c -> f c
  Nothing -> Nothing
  
lookupSupply :: (Ord a) => a -> CurrencySupply a -> Maybe Currency
lookupSupply = lookupSupplyFor (\a -> Just a)

lookupSupplyValue :: (Ord a) => a -> CurrencySupply a -> Maybe Float
lookupSupplyValue = lookupSupplyFor (\(Currency {value = v}) -> Just v)

lookupSupplyName :: (Ord a) => a -> CurrencySupply a -> Maybe String
lookupSupplyName = lookupSupplyFor (\(Currency {name = n}) -> Just n)

lookupSupplyAmount :: (Ord a) => a -> CurrencySupply a -> Maybe Int
lookupSupplyAmount = lookupSupplyFor (\(Currency { amount = a}) -> Just a)

-- M.foldr does not come into scope! Why?!
-- lookupSupplyNames :: (Ord a) => CurrencySupply a -> [String]
-- lookupSupplyNames NoMoney = []
-- lookupSupplyNames (SomeMoney (m)) = M.foldr (\(Currency { name = n }) a -> n : a) [] m 
