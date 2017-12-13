data ZInt = Z Nat Nat
data Nat = Zero | S Nat deriving Show
data B = T | F deriving Show
-- wobei Z Zero b eine positive Zahl b entspricht, und Z a Zero die negative Zahl
-- -a ist. Im allgemeinen ist Z a b gleich b-a.

{-Um auf die beiden einzelnen Teile zuzugreifen.-}
first :: ZInt -> Nat
first (Z a _) = a

second :: ZInt -> Nat
second (Z _ b) = b

{-Ueber Fallunterscheidung werden erstmal alle Faelle abgefangen die einfach
sind, zB wenn eine Zahl negativ und die andere positiv ist. Danach wird
auf die schon bekannten Funktionen zurueck gegriffen.-}
maxZ :: ZInt -> ZInt -> ZInt -- berechnet die größte Zahl
maxZ (Z Zero a) (Z _ Zero) = (Z Zero a)
maxZ (Z _ Zero) (Z Zero b) = (Z Zero b)
maxZ (Z Zero a) (Z Zero b) = (Z Zero (maxN a b))
maxZ (Z a Zero) (Z b Zero) = (Z (minN a b) Zero)

{-Wenn die Zahl positiv ist mache nichts, ansonsten drehe sie nur um.-}
absZ :: ZInt -> ZInt -- absoluter Wert einer Zahl
absZ a
    | first a == Zero = a
    | otherwise = (Z Zero (first a))

{-Gucken welches die Zahl ist und nicht das Vorzeichen, danach Vergleich mit
bekannten Verfahren.-}
isTeilerZ :: ZInt -> ZInt -> B -- überprüft, ob die zweite Zahl Teiler der ersten Zahl ist.
isTeilerZ a b
    | first a == Zero && first b == Zero = isTeiler (second a) (second b)
    | first a == Zero && second b == Zero = isTeiler (second a) (first b)
    | second a == Zero && first b == Zero = isTeiler (first a) (second b)
    | second a == Zero && second b == Zero = isTeiler (first a) (first b)

ggtZ :: ZInt -> ZInt -> ZInt -- größter gemeinsamer Teiler
ggtZ a b
    | first a == Zero && first b == Zero = (Z Zero (ggtN (second a) (second b)))
    | first a == Zero && second b == Zero = (Z Zero (ggtN (second a) (first b)))
    | second a == Zero && first b == Zero = (Z Zero (ggtN (first a) (second b)))
    | second a == Zero && second b == Zero = (Z Zero (ggtN (first a) (first b)))

instance Show ZInt where
    show x = "Z " ++ show (first x) ++ " " ++
             show (second x)

minN :: Nat -> Nat -> Nat -- Minimum Nat
minN a Zero = Zero
minN Zero b = Zero
minN a b
    | S Zero == a = a
    | S Zero == b = b
    | otherwise =  minhelper a b Zero

minhelper :: Nat -> Nat -> Nat -> Nat
minhelper a b s
    |  s == a = a
    |  s == b = b
    | otherwise = minhelper a b (S s)

instance Eq Nat where
    (==) (S x) (S y) = x == y
    (==) Zero Zero = True
    (==) _ _ = False

isTeiler :: Nat -> Nat -> B -- überprüft, ob die zweite Zahl die erste Zahl teilt
isTeiler a b
    | a == b = T
    | b == Zero = F
    | minN a b == b = isTeilerhelper a b (addN b b)
    | otherwise = F

isTeilerhelper :: Nat -> Nat -> Nat -> B
isTeilerhelper a b c
    | a == c = T
    | minN a c == c = isTeilerhelper a b (addN b c)
    | otherwise = F

{-Wir testen nach und nach die Zahlen, teilt bereits eine Zahl die andere
sind wir fertig. Ansonsten gehen wir von der kleineren der beiden Zahlen so
lange runter bis wir bei "1" ankommen. "1" ist immer der kleinste ggt.-}
ggtN :: Nat -> Nat -> Nat
ggtN a b
    | a == Zero && b == Zero = Zero
    | b == Zero = a
    | a == Zero = b
    | isTeiler a b == T = b
    | isTeiler b a == T = a
    | minN a b == a = ggtNhelper b a a
    | otherwise = ggtNhelper a b b

{-kleinerer Wert ist zweiter Wert.-}
ggtNhelper :: Nat -> Nat -> Nat -> Nat
ggtNhelper a b (S c)
    | isTeiler a c == T && isTeiler b c == T = c
    | otherwise = ggtNhelper a b c

instance Eq B where
   (==) (T) (T) = True
   (==) (F) (F) = True
   (==) _ _     = False

addN :: Nat -> Nat -> Nat -- endrekursive Funktionsdefinition fuer die Summe
addN a Zero  = a
addN Zero b  = b
addN a (S b) = S (addN a b)

maxN :: Nat -> Nat -> Nat
maxN a b = iff (ltN a b) b a

iff :: B -> a -> a -> a
iff T a _ = a
iff F _ b = b

ltN :: Nat -> Nat -> B
ltN Zero (S _)  = T
ltN (S a) (S b) = ltN a b
ltN   _    _    = F
