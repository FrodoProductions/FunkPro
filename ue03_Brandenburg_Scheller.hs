{- Frederick Brandenburg, Ferdinand Markward Scheller
Tutoriumsnummer: 03 -}

-- AUFGABE 4

fromDecTo :: Int -> Int -> [Int]
fromDecTo b num = reverse (dec2 b num)

dec2 :: Int -> Int -> [Int]
dec2 b num | b<1 || b>9 = error "Base must be between 0 and 10!"
           | num<1 = []
           | otherwise = (mod num b) : dec2 b (div num b)

{- Testlauf

fromDecTo 5 555
> [4,2,1,0]
-}


-- AUFGABE 5

isSorted :: Ord a => (a -> a -> Bool) -> [a] -> Bool
isSorted f (x:[]) = True
isSorted f (x:y:xs) = f x y && isSorted f (y:xs)

{- Testlauf

isSorted (>=) [5,5,4,2,1,1,1]
> True
-}
