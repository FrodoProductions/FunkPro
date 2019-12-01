{- Frederick Brandenburg, Ferdinand Markward Scheller
Tutoriumsnummer: 03 -}

import Data.List

-- AUFGABE 1

-- Laufzeitkomplexität: O(n)+O(n)∈O(n)
-- Zunächst wird ein e als das Ergebnis von calculateFirst definiert.
-- Dieses wird dann als neues Element an die zurückzugebende Liste angefügt und die Funktion
-- ruft sich selber rekursiv auf, wobei die betrachtete Liste durch deleteElem modifiziert wird.
selectSort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
selectSort _ [] = []
selectSort f xs = let
  e = calculateFirst f xs
  in e : (selectSort f (deleteElem e xs))
  where
   -- Laufzeitkomplexität: O(n)
   -- Diese Funktion wendet die eingegebene Funktion auf die jeweils ersten beiden Elmenete
   -- der Liste an und entfernt das "unterlegene" Element. Dieser Prozess wird fortgesetzt,
   -- bis nur noch eines übrig ist, welches dann auch zurückgegeben wird.
   calculateFirst :: Ord a => (a -> a -> Bool) -> [a] -> a
   calculateFirst f (x:[]) = x
   calculateFirst f (x:y:xs) | f x y == True = calculateFirst f (x:xs)
                             | otherwise = calculateFirst f (y:xs)
   -- Laufzeitkomplexität: O(n)
   -- Diese Funktion verwendet die filter Funktion, um eine Liste aller Elemente =/ e zu erstellen.
   deleteElem :: Eq a => a -> [a] -> [a]
   deleteElem e xs = filter (/=e) xs

{- Testlauf

selectSort (<) ['b','g','e','c','a','l','x','r']
> "abceglrx"
-}


-- AUFGABE 2

-- Laufzeitkomplexitäten der Teilfunktionen:
-- tail: O(1), da diese Funktion von der Länge der Liste unabhängig ist.
-- zipWith: O(n), da aus zwei Listen mit Länge n und m jeweils soviele Tupel generiert werden wie die kürzere
-- Liste lang ist (Im worst case sind sie gleich lang).
-- and: O(n), dabei einer Liste der Länge n immer n-1 Vergleiche durchgeführt werden müssen.

-- Daher: O(n)


-- AUFGABE 3

-- mult:
-- Die Anzahl der Operationen ist gleich m, da in jedem Schritt mit m>0
-- die Funktion mit m-1 aufgerufen wird.
-- Daher O(n).

-- russMult:
-- m wird, falls ungerade, zunächst per ganzzahliger Division zu einer geraden Zahl umgewandelt.
-- Danach nimmt der Wert von m logarithmisch ab, da m immer wieder durch 2 geteilt wird.
-- russMult kann also mit O(log2 n) approximiert werden.


-- AUFGABE 4

-- b)

isSorted :: (Ord a) => (a->a->Bool) -> [a] -> Bool
isSorted cmp xs = and (zipWith cmp xs (tail xs))

traceBubbleSort :: (Ord a) => [a] -> [[a]]
traceBubbleSort xs | isSorted (<=) xs = [xs]
                   | otherwise = traceBubbleSort (moveBubble xs) ++ [xs]
                    where
                    moveBubble [] = []
                    moveBubble [x] = [x]
                    moveBubble (x:y:rest) | (<=) x y = x: moveBubble (y:rest)
                                          | otherwise = y: moveBubble (x:rest)

{- Testlauf

traceBubbleSort [8,6,4,2,3,1,5]
> [[1,2,3,4,5,6,8],[2,1,3,4,5,6,8],[2,3,1,4,5,6,8],[4,2,3,1,5,6,8],[6,4,2,3,1,5,8],[8,6,4,2,3,1,5]]
-}


-- AUFGABE 5

-- a)
  -- i)

allSuffixes :: [a] -> [[a]]
allSuffixes [] = []
allSuffixes xs = xs : allSuffixes (tail xs)

  -- ii)

prefix :: Ord a => [a] -> [a] -> [a]
prefix [] ys = []
prefix xs [] = []
prefix (x:xs) (y:ys) | x/=y = []
                     | otherwise = x : prefix xs ys

  -- iii)

largestPrefix :: Ord a => [[a]] -> (Int, [a])
largestPrefix xs = maximum $ [(length x, x) | x <- (prefixList xs)]
 where
   prefixList :: Ord a => [[a]] -> [[a]]
   prefixList (x:y:[]) = [prefix x y]
   prefixList (x:y:xs) = (prefix x y) : prefixList (y:xs)


-- b)

maxLengthRepSeq :: Ord a => [a] -> [a]
maxLengthRepSeq = snd . largestPrefix . sort . allSuffixes
