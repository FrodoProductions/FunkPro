{- Frederick Brandenburg, Ferdinand Markward Scheller
Tutoriumsnummer: 03 -}

-- AUFGABE 3

-- Definition einer Hilfsfunktion factorial für n!
factorial 0 = 1
factorial n = n * factorial (n-1)

euler :: Double -> Double
euler 0 = 1
euler n = 1 / (factorial n) + euler (n-1)
