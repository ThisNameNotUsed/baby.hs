doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

--length' xs = sum [1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

removeOdds ys = [[ x | x <- xs, even x ] | xs <- ys ]

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]

rightTriangles10 = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

rightTriangles100 = [ (a,b,c) | c <- [1..100], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

trianglesRight = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b] ]

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference' :: Double -> Double
circumference' r = 2 * pi * r

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"

factorial2 :: (Integral a) => a -> a
factorial2 0 = 1
factorial2 n = n * factorial2 (n - 1)

head' :: [a] -> a
head' [] = error "Can't call head on any empty list, dummy!"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs 

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0 

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

initials :: String -> String -> String
initials first last = [a] ++ ". " ++ [b] ++ "."
    where a:_ = first
          b:_ = last

initialsPM :: String -> String -> String
initialsPM (a:_) (b:_) =  [a] ++ ". " ++ [b] ++ "."

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in sideArea + 2 * topArea

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x 

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  

repeat' :: a -> [a]  
repeat' x = x:repeat' x  

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' thing [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

qsort' :: (Ord a) => [a] -> [a]
qsort' [] = []
qsort' (x:xs) =
    let small = qsort' [ a | a <- xs, a <= x]
        big = qsort' [ a | a <- xs, a > x]
    in small ++ [x] ++ big