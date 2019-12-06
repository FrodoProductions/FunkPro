{- Frederick Brandenburg, Ferdinand Markward Scheller
Tutoriumsnummer: 03 -}

-- AUFGABE 1

data Length = Foot Double | Inch Double
                          | Yard Double
                          | Mile Double
                          | Cm Double
                          | M Double
                          | Km Double
            deriving Show

foot2cm :: Length -> Length
foot2cm (Foot ft) = Cm(30.48*ft)

inch2cm :: Length -> Length
inch2cm (Inch ic) = Cm(2.54*ic)

yard2m :: Length -> Length
yard2m (Yard yd) = M(yd/1.094)

mile2km :: Length -> Length
mile2km (Mile mi) = Km(mi*1.609)

{- Testlauf

foot2cm (Foot 5.3)
> Cm 161.54399999999998

inch2cm (Inch 10)
> Cm 25.4

yard2m (Yard 42)
> M 38.39122486288848

mile2km (Mile 100)
> Km 160.9
-}


-- AUFGABE 3

data SimpleBT = L | N SimpleBT SimpleBT
 deriving (Eq, Show)

insertLeaves :: Integer -> SimpleBT -> SimpleBT
insertLeaves 0 t = t
insertLeaves i (N lt rt) = (N (insertLeaves i lt) rt)
insertLeaves i L = insertLeaves (i-1) (N L L)


-- AUFGABE 5

-- Definition des Datentyps unter Verwendung zweier Listen
data Queue a = Queue [a] [a]

enqueue :: a -> Queue a -> Queue a
dequeue :: Queue a -> Queue a
isEmpty :: Queue a -> Bool
makeQueue :: Queue a

-- Ein neues Element wird am Anfang der zweiten Liste angefügt.
enqueue a (Queue as bs) = (Queue as (a:bs))

-- Sollte die erste Liste leer sein, wird die zweite umgedreht und in die erste eingesetzt.
-- Nachdem das erste Element der ersten Liste entfernt wurde, wird diese wieder umgekehrt in
-- die zweite Liste eingesetzt, damit das Element auch tatscählcih nicht mehr aufftaucht.
dequeue (Queue as bs) | null as = dequeue (Queue (reverse bs) bs)
                      | otherwise = (Queue (tail as) (reverse (tail as)))

-- Wenn beide Listen leer sind, so ist auch Queue leer.
isEmpty (Queue as bs) | null as && null bs = True
                      | otherwise = False

makeQueue = (Queue [] [])

-- Hier wird die Gleichheit auf Basis der jeweils zweiten Liste untersucht
(=|=) :: Eq a => Queue a -> Queue a -> Bool
(=|=) (Queue _ as) (Queue _ bs) | as == bs = True
                                | otherwise = False

(<|<) :: Queue a -> Queue a -> Bool
(<|<) (Queue _ as) (Queue _ bs) | (length as) < (length bs) = True
                                | otherwise = False

(>|>) :: Queue a -> Queue a -> Bool
(>|>) (Queue _ as) (Queue _ bs) | (length as) > (length bs) = True
                                | otherwise = False

showQueue :: Queue a -> [a]
showQueue (Queue as []) = []
showQueue (Queue as (b:bs)) = b : (showQueue (Queue as bs))

instance Show a => Show (Queue a) where
  show (Queue as bs) = show (showQueue (Queue as bs))

instance Eq a => Eq (Queue a) where
  (==) as bs = (=|=) as bs

instance Ord a => Ord (Queue a) where
  compare as bs | (<|<) as bs = LT
                | (>|>) as bs = GT
                | otherwise = EQ
  (<) as bs = (<|<) as bs
  (>) as bs = (>|>) as bs

-- Mit dieser Funktion wird ein Wort Buchstabe für Buchstabe in eine Queue überführt.
enqueueWord :: String -> Queue Char -> Queue Char
enqueueWord (c:[]) q = enqueue c q
enqueueWord (c:cs) q = enqueueWord cs (enqueue c q)

{- Testlauf

enqueueWord "Hello" makeQueue
> "olleH"
-}

-- dequeueMultiple ruft dequeue i mal auf.
dequeueMultiple :: Eq a => Int -> Queue a -> Queue a
dequeueMultiple i q | isEmpty q = error "Not enough elements in Queue!"
                    | i<1 = error "Number not in range!"
                    | i == 1 = dequeue q
                    | otherwise = dequeueMultiple (i-1) (dequeue q)

{- Testlauf

dequeueMultiple 2 (enqueueWord "Informatik" makeQueue)
> "kitamrof"
-}
