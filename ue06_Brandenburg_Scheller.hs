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
