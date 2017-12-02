{-Implementierung der gegebenen Formel, davor werden eventuell Falsche
Eingaben abgefangen.-}
weekday :: Int -> Month -> Int -> Weekday
weekday day month year
    | day > 31 || day < 1 = error "ungueltiges Datum"
    | month == Apr && day > 30 = error "ungueltiges Datum"
    | month == June && day > 30 = error "ungueltiges Datum"
    | month == Sept && day > 30 = error "ungueltiges Datum"
    | month == Nov && day > 30 = error "ungueltiges Datum"
    | mod year 4 /= 0 && month == Feb && day > 28 = error "ungueltiges Datum"
    | mod year 4 == 0 && mod year 100 /= 0 &&
      month == Feb && day > 29 = error "ungueltiges Datum"
    | mod year 100 == 0 && mod year 400 /= 0 &&
      month == Feb && day > 28 = error "ungueltiges Datum"
    | mod year 100 == 0 && mod year 400 == 0 &&
      month == Feb && day > 29 = error "ungueltiges Datum"
    | result == 0 = Su
    | result == 1 = Mo
    | result == 2 = Tu
    | result == 3 = We
    | result == 4 = Tu
    | result == 5 = Fr
    | result == 6 = Sa
    | otherwise = error "Exception"
    where result = mod (day + x + (31* mnull)`div`12) 7
          mnull = value month + 12 * ((14- value month)`div`12)-2
          x  = y0 + y0`div`4 - y0`div`100 + y0`div`400
          y0 = year - ((14- value month)`div`12)

{-Ab hier stellen wir die geforderten Datentypen zur Verfuegung.
Bei der Abkuerzung der Namen haben wir uns an den Guidelines der
Yale University orientiert:
https://web.library.yale.edu/cataloging/months.htm-}
data Month =  Jan
            | Feb
            | Mar
            | Apr
            | May
            | June
            | July
            | Aug
            | Sept
            | Oct
            | Nov
            | Dec
    deriving (Show, Eq)

value :: Month -> Int
value a
    | a == Jan = 1
    | a == Feb = 2
    | a == Mar = 3
    | a == Apr = 4
    | a == May = 5
    | a == June = 6
    | a == July = 7
    | a == Aug = 8
    | a == Sept = 9
    | a == Oct = 10
    | a == Nov = 11
    | a == Dec = 12
    | otherwise = error "Falscher Wert"

data Weekday = Mo | Tu | We | Th | Fr | Sa | Su
    deriving (Eq, Ord, Show)
