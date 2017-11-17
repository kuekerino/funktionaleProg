{-Durch List Comprehension und der primzahlen Funtion werden die gewünschten
Tupel erstellt. Durch die ganzen Guards werden gleichzeitig alle Fälle
abgefangen, die wir nicht brauchen.
-}
weakGoldbachTriples :: Int -> [(Int,Int,Int)]
weakGoldbachTriples n = [(x,y,z) | x <- takeWhile (<n) primzahlen,
                                   y <- takeWhile (<n) primzahlen,
                                   z <- takeWhile (<n) primzahlen,
                                   n == x+y+z,
                                   x+y+z < (n+1),
                                   x <= y,
                                   y <= z]

{-Zusammensetzung mehrerer Funktionen, wir haben versucht die einzelnen
Funktionen in die einzelnen Zeilen aufzuteilen um das Lesen zu erleichtern.
In der letzten Zeile wird per Map die oben geschriebene Funktion auf eine Liste
angewendet, die alle ungeraden Zahlen enthält. Danach wird hiervon nur das erste
Element genommen, dies passiert aus Tempogründen. Da Haskell lazy evaluiert
wird hier auch abgebrochen, sobald eine mögliche Zerlegung gefunden wurde.
Danach werden die ersten (n-5)/2 Zahlen angeguckt und geguckt, ob die
entstanden Liste auch genau so lang ist. Wenn dies der Fall ist ist die
Behauptung wahr. Falls nicht bricht der Vorgang sogar schon vorher ab.
-}
wGTriplesUntil :: Int -> Bool
wGTriplesUntil 5 = False
wGTriplesUntil 6 = False
wGTriplesUntil 7 = True
wGTriplesUntil 8 = True
wGTriplesUntil 9 = True
wGTriplesUntil n = if (length
                       $ take ((n-5)`div`2)
                       $ map head
                       $ map weakGoldbachTriples [7,9..])
                       == (n-5)`div`2
    then True
    else False

-- Primzahlen Funktion aus den Vorlesungsfolien
primzahlen :: [Int]
primzahlen = sieb [2..]
    where sieb (p:xs) = p:sieb[k | k<-xs, (mod k p)>0]
