data Root3Num = Root3Num {rationalTeil :: Int,
                          erweiterungTeil :: Int}

-- instance Eq Root3Num where
--   compare a b = compare(rational a, erweiterung a) (rational b, erweiterung b)
-- Eq Klasse hat == und /=



rational :: Root3Num -> Int
rational (Root3Num a _) = a

erweiterung :: Root3Num -> Int
erweiterung (Root3Num _ a) = a

-- Addition, Subtraktion Multiplikation


getValue :: Root3Num -> Float
getValue a = 3.12
