doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

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