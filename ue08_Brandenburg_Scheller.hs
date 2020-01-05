{- Frederick Brandenburg, Ferdinand Markward Scheller
Tutoriumsnummer: 03 -}

-- AUFGABE 1

-- a)

{-

IA:      reverse (reverse []) = []
rev.1 -> reverse [] = []
rev.1 -> [] = []

IV: reverse (reverse xs) = xs

IS:       reverse (reverse (x:xs)) = (x:xs)
rev.2  -> reverse (reverse xs ++ [x]) = (x:xs)
       -> [x] ++ reverse (reverse xs) = (x:xs)
IV     -> [x] ++ xs = (x:xs)
(++).2 -> (x:xs) = (x:xs)

       QED

-}

-- b)

{-

IA:       reverse ([] ++ ys) = reverse ys ++ reverse []
rev.1  -> reverse ([] ++ ys) = reverse ys ++ []
(++).1 -> reverse ys = reverse ys

IV: reverse (xs ++ ys) = reverse ys ++ reverse xs

IS:      reverse ((x:xs) ++ ys) = reverse ys ++ reverse (x:xs)
      -> reverse x:(xs ++ ys) = reverse ys ++ reverse (x:xs)
rev.2 -> reverse (xs ++ ys) ++ [x] = reverse ys ++ reverse (x:xs)
IV    -> reverse ys ++ reverse xs ++ [x] = reverse ys ++ reverse (x:xs)
rev.2 -> reverse ys ++ reverse xs ++ [x] = reverse ys ++ reverse xs ++ [x]

      QED

-}

-- c)

{-

IA:       elem a ([] ++ ys) = elem a [] || elem a ys
(++).1 -> elem a ys = elem a [] || elem a ys
elem.1 -> elem a ys = False || elem a ys
       -> elem a ys = elem a ys

IV: elem a (xs ++ ys) = elem a xs || elem a ys

IS:   elem a ((x:xs) ++ ys) = elem a (x:xs) || elem a ys
   -> elem a x:(xs ++ ys)
   -> elem a [x] || elem a (xs ++ ys) = elem a [x] || elem a xs || elem a ys
IV -> elem a [x] || elem a xs || elem a ys = elem a [x] || elem a xs || elem a ys

      QED

-}

-- d)

{-

IA:        (takeWhile p []) ++ (dropWhile p []) = []
takeW.1 -> [] ++ (dropWhile p []) = []
dropW.1 -> [] ++ [] = []
(++).1  -> [] = []

IV: (takeWhile p xs) ++ (dropWhile p xs) = xs

IS:   (takeWhile p (x:xs)) ++ (dropWhile p (x:xs)) = (x:xs)
IV -> (takeWhile p (x:xs)) ++ (dropWhile p (x:xs)) = x : ((takeWhile p xs) ++ (dropWhile p xs))
   -> (takeWhile p (x:xs)) ++ (dropWhile p (x:xs)) = (takeWhile p (x:xs)) ++ (dropWhile p (x:xs))

   QED

-}

-- e)

{-

IA:      map (f . g) [] = map f . map g []
map.1 -> map (f . []) = map f . []
map.1 -> map [] = map []
map.1 -> [] = []

IV: map (f . g) xs = map f . map g xs

IS:      map (f . g) (x:xs) = map f . map g (x:xs)
map.2 -> (f . g) x:map (f . g) xs = map f . map g (x:xs)
      -> (f . g) x:map (f . g) xs = map f (map g (x:xs))
map.2 -> (f . g) x:map (f . g) xs = map f ((g x):map g xs)
      -> (f . g) x:map (f . g) xs = (f . g) x:map (f . g) xs

      QED

-}

-- AUFGABE 2

{-

IA: length (powerset []) = 2^(length [])
-> length [[]] = 2^0
-> 1 = 1

IV: length (powerset xs) = 2^(length xs)

IS:   length (powerset (x:xs)) = 2^(length (x:xs))
   -> length (powerset (x:xs)) = 2^(length xs)*2^(length [x])
   -> length (powerset (x:xs)) = length (powerset xs)*2^(length [x])
   -> length (powerset xs)*length (powerset [x]) = length (powerset xs)*2^(length [x])
IV -> length (powerset xs)*2^(length [x]) = length (powerset xs)*2^(length [x])

   QED

-}
