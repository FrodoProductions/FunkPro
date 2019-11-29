{- Frederick Brandenburg, Ferdinand Markward Scheller
Tutoriumsnummer: 03 -}

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
-- zipWith: O(n), da aus einer Liste mit n Elementen immer 2n Tupel generiert werden.
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
-- russMult kann also mit O(log2 m) approximiert werden.
