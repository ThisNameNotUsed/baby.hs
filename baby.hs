doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

--length' xs = sum [1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

removeOdds ys = [[ x | x <- xs, even x ] | xs <- ys ]

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

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
