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

-- a)

-- Im worst case befindet sich jedes Element maximal weit von seinem sortierten
-- Platz entfernt, es müsste also jedes Element bis zu n mal bewegt werden,
-- um Ornung herzustellen. Die Komplexität kann also mit O(n^2) approximiert werden.

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

-- Diese Funktion fügt immer die aktuelle Liste zu der Rückgabeliste hinzu und
-- ruft sich dann rekursiv selbst auf, wobei die Liste um ihr erstes Element gekürzt wird.
allSuffixes :: [a] -> [[a]]
allSuffixes [] = []
allSuffixes xs = xs : allSuffixes (tail xs)

{- Testlauf

allSuffixes [1,2,3,4,5,6,7,8,9]
> [[1,2,3,4,5,6,7,8,9],[2,3,4,5,6,7,8,9],[3,4,5,6,7,8,9],[4,5,6,7,8,9],[5,6,7,8,9],[6,7,8,9],[7,8,9],[8,9],[9]]
-}

  -- ii)

-- Die jeweils erste Stelle beider Listen wird auf Gleichheit untersucht; ist diese vorhanden,
-- so wird die Stelle zur Rückgabe hinzugefügt, beide Listen werden um die erste Stelle reduziert
-- und es erfolgt ein rekursiver Aufruf.
prefix :: Ord a => [a] -> [a] -> [a]
prefix [] ys = []
prefix xs [] = []
prefix (x:xs) (y:ys) | x/=y = []
                     | otherwise = x : prefix xs ys

{- Testlauf

prefix [0,0,1,0,0,1,1,1,0,0] [0,0,1,0,0,1,0,0,1,0]
> [0,0,1,0,0,1]
-}

  -- iii)

-- prefixList geht zunächst jedes Paar in der Eingabeliste durch und generiert für jedes das gemeinsame
-- Präfix (falls vorhanden). largestPrefix wählt dann das maximale Element aus dieser Liste aus
-- und gibt es zurück.
largestPrefix :: Ord a => [[a]] -> (Int, [a])
largestPrefix xs = maximum $ [(length x, x) | x <- (prefixList xs)]
 where
   prefixList :: Ord a => [[a]] -> [[a]]
   prefixList (x:y:[]) = [prefix x y]
   prefixList (x:y:xs) = (prefix x y) : prefixList (y:xs)

{- Testlauf

largestPrefix ["aldpoakn","dsfji","ftrdesa","iubu","jidi","jiij","lopkb","oi"]
> (2,"ji")
-}


-- b)
-- Diese Funktion bildet zunächst alle Suffixe der Eingabeliste und sortiert diese. Wenn es Widerholungen in der
-- ursprünglichen Liste gab, so stehen sie nun nebeneinander. Nun wird einfach das längste Prefix gesucht und ausgegeben.
maxLengthRepSeq :: Ord a => [a] -> [a]
maxLengthRepSeq = snd . largestPrefix . sort . allSuffixes

{- Testlauf

maxLengthRepSeq "Loremipsumdolorsitametconsetetursadipscingelitrseddiamnonumyeirmodtemporinviduntutlaboreetdoloremagnaaliquyamerat"
> "dolor"
-}


-- c)
-- allSuffixes: Aus einer Liste der Länge n werden n Teillisten gebildet, daher O(n).
-- prefix: Im worst case sind beide Eingaben unendlich und jedes Element muss untersucht werden: O(n).
-- largestPrefix: In prefixList finden n-1 Untersuchungen der Liste statt, daher O(n),
-- anschließend wird die neue Liste geneiert und jedes Element untersucht, es finden also
-- 2n Reduktionen statt, also O(n).
-- maxLengthRepSeq: O(n) (allSuffixes) + O(n) (sort) + O(n) (largestPrefix) + O(1) (snd) ∈ O(n)
