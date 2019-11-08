{- Frederick Brandenburg, Ferdinand Markward Scheller
Tutoriumsnummer: 03 -}

-- AUFGABE 2

sumOfTeiler :: Int -> Int -> Int -> Int
-- Wenn das Programm bei 0 ankommt, wird es terminiert,
-- da kein weiterer Funktionsaufruf ausgeführt wird
sumOfTeiler 0 a b = 0
-- Wenn n durch a oder b teilbar ist, wird sumOfTeiler
-- rekursiv mit n-1 aufgerufen und zu n addiert,
-- ansonsten wird nur sumOfTeiler mit n-1 aufgerufen,
-- ohne dass das Ergebnis addiert wird.
sumOfTeiler n a b | n `mod` a == 0 || n `mod` b == 0 = n + sumOfTeiler (n-1) a b
                  | otherwise = sumOfTeiler (n-1) a b


-- AUFGABE 3

-- Definition einer Hilfsfunktion factorial,
-- damit n! einfach berechnet werden kann.

factorial 0 = 1
factorial n = n * factorial (n-1)

euler :: Double -> Double
euler 0 = 1
euler n = 1 / (factorial n) + euler (n-1)


-- AUFGABE 5

-- Da trueDivisor immer eine Referenz auf seinen Anfangspunkt (das ursprüngliche n)
-- behalten muss und ich keine Möglichkeit gefunden habe, wie dies mit einer
-- rekursiven Funktion mit nur einem Argument möglich sein könnte, habe ich
-- die eigentliche Funktionalität in die zweiargumentige Funktion recursiveDiv
-- geschrieben, in der das eine Argument n zum zählen verwendet und das andere
-- nicht verändert wird und daher in der Rekursion seinen Originalwert behält.

trueDivisor :: Int -> [Int]
trueDivisor n = reverse (recursiveDiv n n)

recursiveDiv :: Int -> Int -> [Int]
recursiveDiv 0 a = []
recursiveDiv n a | a `mod` n == 0 && n /= a = n : (recursiveDiv (n-1) a)
                 | otherwise = (recursiveDiv (n-1) a)


-- AUFGABE 6

true :: Int
true = 1

false :: Int
false = 0

negation :: Int -> Int
negation x = -(x-1)

und :: Int -> Int -> Int
und b c = b*c

oder :: Int -> Int -> Int
oder b c = b + c - b * c

exoder :: Int -> Int -> Int
exoder b c = b + c - 2 * b * c

-- Diese Funktion überprüft immer das erste Element beider Listen mithilfe der exoder-Funktion.
-- Wenn exoder == 1 gilt, die Elemente also unterschiedlich sind, addiert die Funktion
-- 1 zu der Rückgabe, entfernt die untersuchten Elemente und ruft sich selbst Rekursiv mit den
-- nun gekürzten Listen auf. Dies geschieht auch in dem Fall, dass die beiden Elemente gleich sind,
-- mit der Ausnahme, dass natürlich der Rückgabewert nicht erhöht wird.

hamming_distance :: [Int] -> [Int] -> Int
hamming_distance [] [] = 0
hamming_distance as bs | length as /= length bs = error "The two numbers have to have the same length!"
                       | exoder (head as) (head bs) == 1 = 1 + hamming_distance (drop 1 as) (drop 1 bs)
                       | otherwise = hamming_distance (drop 1 as) (drop 1 bs)
