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


-- AUFGABE 6

reverseDigits :: Int -> Int
reverseDigits n = listToInt( reverse (intToList n))

intToList :: Int -> [Int]
intToList 0 = []
intToList n = intToList (n `div` 10) ++ [n `mod` 10]

listToInt :: [Int] -> Int
listToInt [] = 0
listToInt (x:xs) = x * 10^(length xs) + listToInt xs

{- Testlauf

reverseDigits 123456789
> 987654321
-}


-- AUFGABE 7

poss :: Eq a => a -> [a] -> [Int]
poss e xs = atPos e 0 xs

atPos :: Eq a => a -> Int -> [a] -> [Int]
atPos e n es | n >= (length es) = []
             | e == (es !! n) = n : atPos e (n+1) es
             | otherwise = atPos e (n+1) es

{- Testlauf

poss 'L' ['H','A','L','L','O']
> [2,3]
-}
