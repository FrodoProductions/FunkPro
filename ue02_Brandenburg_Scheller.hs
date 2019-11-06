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
