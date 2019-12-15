{- Frederick Brandenburg, Ferdinand Markward Scheller
Tutoriumsnummer: 03 -}

-- AUFGABE 1

-- Alle Hilfsfunktionen

data B = T | F
 deriving Show

data Nat = Zero | S Nat
 deriving Show


(<<) :: Nat -> Nat -> B
(<<) Zero (S _) = T
(<<) (S a) (S b) = (<<) a b
(<<) _ _ = F

addN :: Nat -> Nat -> Nat
addN a Zero = a
addN a (S b) = S(addN a b)


foldn :: (Nat -> Nat) -> Nat -> Nat -> Nat
foldn h c Zero = c
foldn h c (S n) = h (foldn h c n)

predN :: Nat -> Nat
predN Zero = Zero
predN (S n) = n


andB :: B -> B -> B
andB T T = T
andB _ _ = F

orB :: B -> B -> B
orB F F = F
orB _ _ = T

iff :: B -> a -> a -> a
iff T n m = n
iff F n m = m

data ZInt = Z Nat Nat

zadd :: ZInt -> ZInt -> ZInt
zadd (Z a b) (Z c d) = Z (addN a b) (addN c d)

zsub :: ZInt -> ZInt -> ZInt
zsub (Z a b) (Z c d) = Z (addN a d) (addN b c)

-- a)

eqB :: B -> B -> B
eqB T T = T
eqB F F = T
eqB _ _ = F

notB :: B -> B
notB T = F
notB F = T

xorB :: B -> B -> B
xorB T F = T
xorB F T = T
xorB _ _ = F

eqN :: Nat -> Nat -> B
eqN Zero Zero = T
eqN Zero _ = F
eqN _ Zero = F
eqN (S a) (S b) = andB (notB ((<<) (S a) (S b))) (notB ((<<) (S b) (S a)))

oddN :: Nat -> B
oddN Zero = F
oddN (S Zero) = T
oddN (S(S n)) = oddN n

fibonacci :: Nat -> Nat
fibonacci Zero = Zero
fibonacci (S Zero) = (S Zero)
fibonacci (S(S n)) = addN (fibonacci (S n)) (fibonacci n)

isTeilerN :: Nat -> Nat -> B
isTeilerN _ Zero = F 
isTeilerN Zero _ = T
isTeilerN a (S b) = orB (eqN (subN a (S b)) (S b)) (andB (notB ((<<) a (S b))) (isTeilerN (subN a (S b)) (S b)))

ggtN :: Nat -> Nat -> Nat
ggtN _ Zero = error "Illegal Input"
ggtN Zero b = b
ggtN a b = iff ((<<) a b) (ggtNhelp b a a) (ggtNhelp a b b)

ggtNhelp :: Nat -> Nat -> Nat -> Nat
ggtNhelp a b Zero = error"no ggt exists"
ggtNhelp a b (S h) = iff ((andB (isTeilerN a (S h)) (isTeilerN b (S h)))) (S h) (ggtNhelp a b h)

-- b)

subN :: Nat -> Nat -> Nat
subN n m = foldn predN n m

-- c)

eqZ :: ZInt -> ZInt -> B
eqZ (Z a b) (Z c d) = iff (eqN (betragZ (absZ(Z a b))) (betragZ(absZ (Z c d)))) (iff (xorB ((<<) a b) (notB((<<) c d))) T F) F

(<<<) :: ZInt -> ZInt -> B
(<<<) (Z a b) (Z c d) = iff ((<<) a b) (iff ((<<) c d) (iff ((<<) (betragZ (absZ(Z c d))) (betragZ(absZ (Z a b)))) T F) T) (iff ((<<) (betragZ (absZ(Z a b))) (betragZ (absZ(Z c d)))) T F)

negZ :: ZInt -> ZInt
negZ (Z a b) = iff (a << b) (Z a b) (Z b a)

maxZ :: ZInt -> ZInt -> ZInt
maxZ (Z a b) (Z c d) = iff ((<<<) (Z a b) (Z c d)) (Z c d) (Z a b)

multZ :: ZInt -> ZInt -> ZInt
multZ (Z a b) (Z c d) = iff (xorB ((<<) a b) ((<<) c d))  (Z Zero ((multN (betragZ (absZ(Z a b))) (betragZ (absZ(Z c d)))))) (Z (multN (betragZ (absZ(Z a b))) (betragZ (absZ(Z c d)))) Zero)

absZ :: ZInt -> ZInt
absZ (Z a b) = iff (a << b) (Z b a) (Z a b)

powZ :: ZInt -> Nat -> ZInt
powZ (Z a b) Zero = (Z a b)
powZ (Z a b) (S n) = powZ (multZ (Z a b) (Z a b)) n

isTeilerZ :: ZInt -> ZInt -> B
isTeilerZ (Z a b) (Z c d) = isTeilerN (betragZ (absZ(Z a b))) (betragZ (absZ(Z c d)))

ggtZ :: ZInt -> ZInt -> ZInt
ggtZ (Z a b) (Z c d) = (Z (ggtN (betragZ (absZ(Z a b))) (betragZ (absZ(Z c d)))) Zero) 

multN :: Nat -> Nat -> Nat
multN Zero (S a) = Zero
multN (S Zero) (S a) = (S a)
multN a b = multNhelp a b Zero

multNhelp :: Nat -> Nat -> Nat -> Nat
multNhelp Zero b acc = acc
multNhelp (S a) b acc = multNhelp a b (addN b acc)

betragZ :: ZInt -> Nat
betragZ (Z a b) = subN a b

-- d)

zint2Int :: ZInt -> Int
zint2Int (Z Zero Zero) = 0
zint2Int (Z Zero (S n)) = (zint2Int (Z Zero n)) - 1
zint2Int (Z (S n) m) = (zint2Int (Z n m)) + 1

int2zint :: Int -> ZInt
int2zint i = int2zintHelp i (Z Zero Zero)
 where
  int2zintHelp :: Int -> ZInt -> ZInt
  int2zintHelp i (Z n m) | i<0 = int2zintHelp (i+1) (Z n (S m))
                         | i>0 = int2zintHelp (i-1) (Z (S n) m)
                         | otherwise = (Z n m)

-- e)

instance Show ZInt where
  show z = show (zint2Int z)


-- AUFGABE 2

data BSearchTree a = Nil | Node a (BSearchTree a) (BSearchTree a)
 deriving (Show, Eq)

mapTree :: (Ord a, Ord b) => (a -> b) -> BSearchTree a -> BSearchTree b
mapTree f (Node x Nil Nil) = (Node (f x) Nil Nil)
mapTree f (Node x lt Nil) = (Node (f x) (mapTree f lt) Nil)
mapTree f (Node x Nil rt) = (Node (f x) Nil (mapTree f rt))
mapTree f (Node x lt rt) = (Node (f x) (mapTree f lt) (mapTree f rt))


-- AUFGABE 3

-- p=Terminator
-- f=Funktion
-- g=Elementmanipulator
-- x=Element

unfold p f g x | p x = []
               | otherwise = f x : unfold p f g (g x)

unfoldMap :: (a -> b) -> [a] -> [b]
unfoldMap f xs = unfold null (f . (head)) (tail) xs

alwaysFalse :: a -> Bool
alwaysFalse x = False

unfoldIterate :: (a -> a) -> a -> [a]
unfoldIterate f x = unfold alwaysFalse f f x

unfoldDec2bin :: Int -> [Int]
unfoldDec2bin n = reverse(unfold zero (`mod` 2) (`quot` 2) n)
 where
   zero n = n==0
