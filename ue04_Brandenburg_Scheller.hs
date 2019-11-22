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


-- AUFGABE 4

-- Diese Funktion untersucht im Bereich von 2 bis n jede Zahl darauf, ob die Summer ihrer Teiler
-- dem Doppelten dieser Zahl entspricht. Wenn ja, wird sie der Ausgabe hinzugefügt.
perfectsUntil :: Int -> [Int]
perfectsUntil n = [x | x<-[2..n], sum (listOfDivs x) == (x*2)]

-- Diese Hilfsfunktion gibt für eine gegebene Zahl eine Liste aller seiner Teiler aus,
-- in dem sie mit mod jede Zahl zwischen 1 und n auf Teilbarkeit untersucht.
listOfDivs :: Int -> [Int]
listOfDivs n = [x | x<-[1..n], n `mod` x == 0]

{- Testlauf

perfectsUntil 100
> [6,28]
-}


-- AUFGABE 5

-- Zunächst wird mit filter eine neue Liste generiert, welche nur nur jede Instanz des gesuchten
-- Elements enthält. Danach wird die Länge dieser Liste ausgegeben (fromIntegral ist nötig, um den von
-- length zurückgebenen Int in einen Num-Typen umzuwandeln)
counts :: (Num a, Eq b) => b -> [b] -> a
counts e es = fromIntegral (length (filter (== e) es))

{- Testlauf

counts 1 [0,0,1,0,1,0,1,0]
> 3
-}

-- Hier wird mit filter eine neue Liste generiert, welche nur nur jedes Element enthält, welches
-- die angegebene Bedingung erfüllt. Danach wird untersucht, ob diese Liste genau ein Element enthält.
single :: (a -> Bool) -> [a] -> Bool
single f xs = length (filter f xs) == 1

{- Testlauf

single (>7) [1,2,3,4,5,6,7,8]
> True
-}

-- Hier wird mit filter eine neue Liste generiert, welche nur nur jedes Element enthält, welches
-- die angegebene Bedingung erfüllt. Danach wird untersucht, ob die Länge dieser Liste größer als die Hälfte der
-- Länge der ursprünglichen Liste ist.
mostly :: (a -> Bool) -> [a] -> Bool
mostly f xs = length (filter f xs) > (length xs) `div` 2

-- bin2dec :: [Integer] -> Integer
-- bin2dec xs =

{- Testlauf

mostly (<4) [1,2,3,4,5,6,7,8,9,10]
> False
-}
