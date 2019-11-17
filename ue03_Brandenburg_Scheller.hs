{- Frederick Brandenburg, Ferdinand Markward Scheller
Tutoriumsnummer: 03 -}

-- AUFGABE 1

okt2hex xs = reverse (bin2hex (okt2bin (reverse (['0'] ++ xs))))

okt2bin :: [Char] -> [Char]
okt2bin [] = []
okt2bin (n:ns)
 | n=='0' = ['0','0','0'] ++ okt2bin ns
 | n=='1' = ['1','0','0'] ++ okt2bin ns
 | n=='2' = ['0','1','0'] ++ okt2bin ns
 | n=='3' = ['1','1','0'] ++ okt2bin ns
 | n=='4' = ['0','0','1'] ++ okt2bin ns
 | n=='5' = ['1','0','1'] ++ okt2bin ns
 | n=='6' = ['0','1','1'] ++ okt2bin ns
 | n=='7' = ['1','1','1'] ++ okt2bin ns
 | otherwise = error "Octal input not in range!"

bin2hex :: [Char] -> [Char]
bin2hex [] = []
bin2hex (x:[]) = []
bin2hex (x:y:[]) = []
bin2hex (x:y:z:[]) = []
bin2hex xs = case (take 4 xs) of
   ['0','0','0','0'] -> '0' : bin2hex (drop 4 xs)
   ['1','0','0','0'] -> '1' : bin2hex (drop 4 xs)
   ['0','1','0','0'] -> '2' : bin2hex (drop 4 xs)
   ['1','1','0','0'] -> '3' : bin2hex (drop 4 xs)
   ['0','0','1','0'] -> '4' : bin2hex (drop 4 xs)
   ['1','0','1','0'] -> '5' : bin2hex (drop 4 xs)
   ['0','1','1','0'] -> '6' : bin2hex (drop 4 xs)
   ['1','1','1','0'] -> '7' : bin2hex (drop 4 xs)
   ['0','0','0','1'] -> '8' : bin2hex (drop 4 xs)
   ['1','0','0','1'] -> '9' : bin2hex (drop 4 xs)
   ['0','1','0','1'] -> 'A' : bin2hex (drop 4 xs)
   ['1','1','0','1'] -> 'B' : bin2hex (drop 4 xs)
   ['0','0','1','1'] -> 'C' : bin2hex (drop 4 xs)
   ['1','0','1','1'] -> 'D' : bin2hex (drop 4 xs)
   ['0','1','1','1'] -> 'E' : bin2hex (drop 4 xs)
   ['1','1','1','1'] -> 'F' : bin2hex (drop 4 xs)
   otherwise -> error "Binary not in range!"

{- Testlauf

okt2hex "25476"
> "2B3E"
-}

-- AUFGABE 2

ggt_of :: [Int] -> Int
ggt_of [] = 0
ggt_of (x:xs) = euklid x (ggt_of xs)

euklid :: Int -> Int -> Int
euklid x y = if y /= 0 then euklid y (mod x y) else x

{- Testlauf

[15, 30, 600, 915]
> 15
-}

-- AUFGABE 3

balance :: [Char] -> Bool
balance text = bal [] text
 where
   bal :: [Char] -> [Char] -> Bool
   bal [] [] = True
   bal stapel ('(':xs) = bal (')':stapel) xs
   bal stapel ('[':xs) = bal (']':stapel) xs
   bal stapel ('{':xs) = bal ('}':stapel) xs
   bal (s:stapel) (x:xs) | x/='(' && x/='[' && x/='{' && x/=')' && x/=']' && x/='}' = bal (s:stapel) xs
                         | s==x = bal stapel xs
   bal _ _ = False

{- Testlauf

balance "54+(a-[4,9))"
> False
-}


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
