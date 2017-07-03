-- Solutions for chapter 6 of first part

sumDown :: Int -> Int
sumDown 0 = 0
sumDown x = (+) x $ sumDown (x-1)

myExponent :: Int -> Int -> Int
myExponent a 0 = 1
myExponent a 1 = a
myExponent a 2 = a*a
myExponent a b | even b = myExponent a (b `div` 2) * myExponent a (b`div`2)
               | otherwise = myExponent a (b-1) * myExponent a 1

myGcd :: Int -> Int -> Int 
myGcd a 0 = a
myGcd a 1 = 1
myGcd a b | b > a = myGcd b a
          | a == b = a
          | otherwise = myGcd (a `mod` b) b
          
                    

myAnd :: [Bool] -> Bool
myAnd [] = error "Empty List!"
myAnd (x:[]) = x
myAnd (x:xs) | x  = myAnd xs
             | otherwise = False
             
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs:xss) = xs ++ myConcat xss

--Merge sort 
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x > y = y : merge (x:xs) ys
                    | otherwise = x : merge xs (y:ys)

split :: [a] -> ([a], [a])
split myList = splitAt (((length myList) + 1) `div` 2) myList


mSort :: (Ord a) => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort xs = merge (mSort $ fst tup) (mSort $ snd tup)
    where tup = split xs

    
