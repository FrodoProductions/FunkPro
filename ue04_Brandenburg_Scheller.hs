{- Frederick Brandenburg, Ferdinand Markward Scheller
Tutoriumsnummer: 04 -}

-- AUFGABE 1

{-
[(n,m) | n<-[1..3], m<-[3,2..0], n/=m]

Zunächst werden für alle n m Tupel generiert:

[(1,m),
(1,m),
(1,m),
(1,m),
(2,m),
(2,m),
(2,m),
(2,m),
(3,m),
(3,m),
(3,m),
(3,m)]

Nun werden in diese m eingesetzt:

[(1,3),
(1,2),
(1,1),
(1,0),
(2,3),
(2,2),
(2,1),
(2,0),
(3,3),
(3,2),
(3,1),
(3,0)]

Jetzt werden alle Tupel enfernt bei denen n==m:

[(1,3),
(1,2),
(1,0),
(2,3),
(2,1),
(2,0),
(3,2),
(3,1),
(3,0)]

-}


-- AUFGABE 5

counts :: (Num a, Eq b) => b -> [b] -> a
counts e es = fromIntegral (length (filter (== e) es))

single :: (a -> Bool) -> [a] -> Bool
single f xs = length (filter f xs) == 1

mostly :: (a -> Bool) -> [a] -> Bool
mostly f xs = length (filter f xs) > (length xs) `div` 2

bin2dec :: [Integer] -> Integer
-- bin2dec xs =
