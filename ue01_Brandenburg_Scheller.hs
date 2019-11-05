{- Frederick Brandenburg, Ferdinand Markward Scheller
Tutoriumsnummer: 03 -}


-- AUFGABE 1, ABfrage aller möglichen Klammertypen mit case-Anweisung

isParenthesis :: Char -> Bool
isParenthesis s = case s of
  '[' -> True
  ']' -> True
  '(' -> True
  ')' -> True
  '{' -> True
  '}' -> True
  otherwise -> False

{- Testlauf

> isParenthesis '['
True
-}




-- AUFGABE 2

-- a)

leap_year_a :: Int -> Bool
leap_year_a y = if y`mod`400 == 0
  then True
  else if y`mod`100 == 0 && y`mod`400 /= 0
    then False
    else if y`mod`4 == 0 && y`mod`100 /= 0
      then True
      else False

{- Testlauf

> leap_year_a 2019
False
> leap_year_a 2004
True
-}


-- b)

leap_year_b :: Int -> Bool
leap_year_b y
 | y`mod`400 == 0 = True
 | y`mod`100 == 0 && y`mod`400 /= 0 = False
 | y`mod`4 == 0 && y`mod`100 /= 0 = True
 | otherwise = False

 {- Testlauf

 > leap_year_a 2000
 True
 > leap_year_a 2300
 False
 -}


 -- c)

leap_year_c :: Int -> Bool
leap_year_c y = if a then True else if b then False else if c then True else False
 where
  a = y`mod`400 == 0
  b = y`mod`100 == 0 && y`mod`400 /= 0
  c = y`mod`4 == 0 && y`mod`100 /= 0

{- Testlauf

> leap_year_a 1980
True
> leap_year_a 1453
False
-}




-- AUFGABE 3

weekDay :: Int -> Int -> Int -> String
weekDay day month year
-- Kontrolle der Wertbereiche nach Aufgabe b)
 | 0 > day || day > 31 = error "Day invalid"
 | 0 > month || month > 12 = error "Month invalid"
 | 0 > year = error "Year invalid"
 | otherwise = case wd of
  -- Aufgabe c)
  0 -> "Sonntag"
  1 -> "Montag"
  2 -> "Dienstag"
  3 -> "Mittwoch"
  4 -> "Donnerstag"
  5 -> "Freitag"
  6 -> "Samstag"
 where
  wd = mod (day + x + (31 * m0) `div` 12) 7
  y0 = year - ((14-month) `div` 12)
  x = y0 + y0 `div`4 - y0 `div` 100 + y0 `div` 400
  m0 = month + 12 * ((14-month) `div` 12) - 2


-- AUFGABE 4

paintChars f size = putStrLn (genChars f size)

genChars :: ((Int, Int, Int) -> Char) -> Int -> [Char]
genChars f size = paint size (map f [(x,y,size) | y <- [1..size], x <- [1..size]])
                      where
                        paint 0  []     = []
                        paint 0 (c:cs)  = '\n' : (paint size (c:cs))
                        paint n (c:cs)  = c: (paint (n-1) cs)


diags (x,y,size)
-- Nicht wirklich elegant, aber naja
 | x==y = ' '
 | x==(y+durchVier) = ' '
 | x==(y+durchVier*2) = ' '
 | x==(y+durchVier*3) = ' '
 | y==(x+durchVier) = ' '
 | y==(x+durchVier*2) = ' '
 | y==(x+durchVier*3) = ' '
 | otherwise = 'o'
 where durchVier = size `div` 4

rectangles (x,y,size)
 |x<=d = '*'
 |x>d && x<=d*2 && y>d = '8'
 |x>d*2 && y>d*2 = '|'
 |otherwise = ' '
 where
   d = size`div`3

{- Anwendung der Kreisvariante von Bresenhams Algorithmus;
Auf jede Koordinate, für die gilt x^2 + y^2 <= r^2 wird ein Char gezeichnet. Dieser Algorithmus geht jedoch im Original von einem
Koordinatensystem mit (0,0) als Zentrum aus, daher wird von jeder Koordinate der Radius abgezogen,
um solch ein Koordinatensystem zu simulieren. -}
circles (x,y,size)
 | (x-r)^2+(y-r)^2 <= s^2 = ' '
 | (x-r)^2+(y-r)^2 <= r^2 = '.'
 | otherwise = '*'
 where
   s = size `div` 4 + 1
   r = size `div` 2 + 1

circle (x,y,size)
 -- Nicht sehr elegant, aber es funktioniert.
 -- Definition des Kreises in der Mitte
 | (x-r)^2+(y-r)^2 <= s^2 = ' '
 -- "Sektor" oben links
 | y <= x && x <= r = '#'
 -- "Sektor" oben rechts (Es wird hier von der Größe des Bildes x abgezogen,
 -- damit sich die y-Koordinaten immer weiter dem oberen Rand annähern)
 | x>r && y <= size-x+1 = '#'
 -- Unterer rechter "Sektor", an sich nur Umkehrung des ersten Guards
 | y >= x && x >= r = '#'
 -- Unterer linker Sektor
 | y>r && x<r && x >= size-y+1 = '#'
 -- Der noch nicht durch den Kreis oder die Sektoren belegte Teil des Bildes
 -- wird nun mit Punkten gefüllt.
 | otherwise = '.'
 where
   s = size `div` 4 + 1
   r = size `div` 2 + 1
