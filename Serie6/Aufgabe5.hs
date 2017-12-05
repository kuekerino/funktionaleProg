data B = T | F deriving Show
data Nat = Zero | S Nat deriving Show

{-Instanzierungen der Eq Klasse um Vergleiche anstellen zu koennen.-}
instance Eq B where
     (==) (T) (T) = True
     (==) (F) (F) = True
     (==) _ _     = False

instance Eq Nat where
    (==) (S x) (S y) = x == y
    (==) Zero Zero = True
    (==) _ _ = False

{-Dadurch, dass wir die Eq Klasse instanziert haben koennen wir hier direkt
den == Operator benutzen.-}
eqB :: B -> B -> B
eqB a b
    | a == b = T
    | otherwise = F

xorB :: B -> B -> B
xorB a b
    | a /= b = T
    | otherwise = F

{-Definition der Addition fuer die natuerlichen Zahlen.-}
addN :: Nat -> Nat -> Nat -- endrekursive Funktionsdefinition fuer die Summe
addN a Zero  = a
addN Zero b  = b
addN a (S b) = S (addN a b)

{-Wir zaehlen von Null hoch, die erste Zahl, die wir erreichen ist auch das
Minimum.-}
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

{-Wir addieren so lange die zweite Zahl auf sich selber, bis die Zahlen
entweder gleich sind, dann ist sie ein Teiler oder die zweite Zahl groesser
als die erste wird, dann ist kein Teiler.-}
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

-- unter Verwendung von foldn aus der VL
{-erst werden alle speziellen Potenzfaelle abgefangen und falls normal
gerechnet werden muss, die Multiplikation als Potenz ausgefuehrt.-}
powN :: Nat -> Nat -> Nat -- Potenz für natürliche Zahlen a^b
powN a b
    | a == Zero && b == Zero = error "nicht definiert"
    | b == Zero = S Zero
    | a == Zero = Zero
    | a == (S Zero) = (S Zero)
    | b == (S Zero) = a
    | otherwise = foldn (mulN a) a c
        where (S c) = b

{-Hilfsfunk um die Multiplikation auszulagern und alles uebersichtlicher zu
machen.-}
mulN :: Nat -> Nat -> Nat
mulN a b
    | a == Zero || b == Zero = Zero
    | a == (S Zero) = b
    | b == (S Zero) = a
    | otherwise = foldn (addN a) a c
        where (S c) = b

{-Unter Zuhilfenhame der Vorganegnerfunktion wird oft genug der Vorganenger
gesucht und zurueck gegeben, falls b <= a.-}
subN :: Nat -> Nat -> Nat -- Subtraktion (subN a b => Zero, wenn b>a)
subN a b
    | minN a b == a = Zero
    | otherwise = foldn (predecessor) a b

{-Vorgaengerfunktion.-}
predecessor :: Nat -> Nat
predecessor a
    | a == Zero = error "Zero hat keinen Vorgaenger"
    | otherwise = b
        where (S b) = a

foldn :: (Nat -> Nat) -> Nat -> Nat -> Nat
foldn h c Zero  = c
foldn h c (S n) = h (foldn h c n)
