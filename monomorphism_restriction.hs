doubleMe x = x + x

f1 x = show x

{-- 
f2 = \x -> show x (does not work)
--}

f3 :: (Show a) => a -> String
f3 = \x -> show x

{-- 
f4 = show (does not work)
--}

f5 :: (Show a) => a -> String
f5 = show