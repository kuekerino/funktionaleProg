data Root3Num = Root3Num Int Int

{-Implementierung der Eq Klasse-}
instance Eq Root3Num where
    a == b = rationalTeil a == rationalTeil b &&
             erweiterungTeil a == erweiterungTeil b
    a /= b = rationalTeil a /= rationalTeil b ||
             erweiterungTeil a /= erweiterungTeil b

{-Hilfunktionen, mit deren Hilfe wir uns die einzelnen Teile holen koennen.-}
rationalTeil :: Root3Num -> Int
rationalTeil (Root3Num a _) = a

erweiterungTeil :: Root3Num -> Int
erweiterungTeil (Root3Num _ b) = b

{-Implementierung der show Klasse.-}
instance Show Root3Num where
    show x = "Root3Num " ++ show (rationalTeil x) ++ " " ++
             show (erweiterungTeil x)

{-Definition der geforderten Operatoren, eine Erweiterung fuer die
Operatoren, die eine Warnung werfen, nahmen wir nicht vor, da sie erstens nicht
gefordert waren und zweites eine Verallgemeinerung auf Zerfaellungskoerper
von den fehleden Operatoren nicht sinnvoll ist.-}
instance Num Root3Num where
    a + b = Root3Num (rationalTeil a + rationalTeil b)
                     (erweiterungTeil a + erweiterungTeil b)
    a - b = Root3Num (rationalTeil a - rationalTeil b)
                     (erweiterungTeil a - erweiterungTeil b)
    a * b = Root3Num (rationalTeil a * rationalTeil b +
                      erweiterungTeil a * erweiterungTeil b * 3)
                     (rationalTeil a * erweiterungTeil b +
                      erweiterungTeil a * rationalTeil b)

{-Implementierung der getValue Funktion. Es wird einfach der Wert ausgerechnet.
-}
getValue :: Root3Num -> Float
getValue a = fromIntegral(rationalTeil a) +
             fromIntegral (erweiterungTeil a ) * sqrt 3
