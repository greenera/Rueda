
--ulaz: putanja do .txt fajla u kome se nalazi spisak figura sa potrebnim informacijama
--izlaz: niz figura (torki) tako da svakom polju torke odgovara po jedna informacija iz fajla 
--svaka linija sadrzi niz informacija o jednoj figuri, a informacije u fajlu su razdvojene dvotackom
--komentari su u fajlu obelezeni sa "--" na pocetku i kraju
--komentari i nepravilno popunjeni redovi se ignorisu
--note: funkcija ce biti sklona dopuni kako bude rastao br. informacija o figurama
ucitajFigure :: String -> [(String, String, String, Int)]


--ulaz: tip figura za izlistavanje (linija, echo, casino, rueda, sve)
--	niz svih figura
--izlaz: ispis odabranih figura na standardni izlaz
--figure koje treba izlistati u zavisnosti od odabrane opcije:
--	1) linija (koriste se figure ciji je prvi element linija)
--	2) echo (koriste se figure ciji je prvi element echo  || prvi linija a drugi echo)
--	3) casino (koriste se figure ciji je prvi element casino)
--	4) rueda (koriste se figure ciji je prvi element rueda || casino || echo || drugi element echo) 
izlistajFigure :: String -> [(String, String, String, Int)] -> IO ()
izlistajFigure x l = putStr (odabrani 1 x l)

--pomocna funkcija koja pravi string prema opisu f-je izlistajFigure
odabrani :: Int -> String -> [(String, String, String, Int)] -> String
odabrani i x [] = ""
odabrani i x (gl@(y, z , ime, _):rep)
        | x == "LINIJA" && x == y =  show i ++ " " ++ ime ++ "\n" ++ odabrani (i+1) x rep
        | x == "ECHO" && (x == y || (y == "LINIJA" && x == z)) = show i ++ " " ++ ime ++ "\n" ++ odabrani (i+1) x rep
        | x == "CASINO" && x == y = show i ++ " " ++ ime ++ "\n" ++ odabrani (i+1) x rep
        | x == "RUEDA" && (x == y || y == "CASINO" || y == "ECHO" || z == "ECHO") = show i ++ " " ++ ime ++ "\n" ++ odabrani (i+1) x rep
        | x == "svi" = show i ++ " " ++ ime ++ "\n" ++ odabrani (i+1) x rep
        | otherwise = odabrani i x rep


--ulaz: figura za dodavanje
--izlaz: True/False (u zavisnosti da li je uspesno dodata)
--opis:
--fja dopusta unos jedino ispravnih vrednosti za prvi i drugi element torke: linija (echo || ""), echo , casino (iz encufle || otvaranja), rueda (echo || casino)
--fja vrsi proveru: da li vec postoji figura sa istim 1. i 3. elementom
--	da li vec ima element u liniji za echo stav ukoliko se nova figura ubacuje u echo (i obrnuto)
--	da li je trajanje figure deljivo sa 8
--ukoliko su svi uslovi zadovoljeni figura se upisuje u txt fajl sa figurama i dodaje se ucitanom nizu figura
dodajFiguru :: [(String, String, String, Int)] -> Bool


--ulaz: (tip figure, naziv figure, nazivFajla)
--izlaz: True/False (u zavisnosti da li je figura uspesno obrisana)
izbrisiFiguru :: (String, String, String) -> Bool


--izlaz: niz odabranih figura
--opis:
--korisniku se daje da bira koje figure zeli da vezba i na osnovu toga se generise lista
--prvo korisnik treba da izabere tip figura [izaberiTip]
--zatim mu se ispisuju imenima figura sa sve rednim brojem [izlistajFigure]
--na kraju korisnik bira redne brojeve figura koje zeli, posle svakog rednog broja navodi koliko cesto na skali od 1 do 11 zeli da se proziva odabrana figura zavrsavajuci niz nulom [izaberiFiguru]
generisiListuZelja :: [(String, String, String, Int)]


--ulaz: 
--izlaz: ime izabranog tipa
--opis: 
--funkcija ispisuje sve moguce tipove i nacin njihovog izbora
--zatim ucitava izabrano
--ukoliko je ispravno vraca izabrani tip
--inace se rekurzivno poziva ponovo
izaberiTip :: String


--ulaz: redniBrojFigure, ocena od 1 do 11
--izlaz: korak sa zadatim rednim brojem
--opis:
--ukoliko je uneti redni broj 0 zavrsava se sa unosom
--ukoloko nije proverava se ima li figure sa tim rendim brojem
--ako ima vraca se ta figura, 
--inace se rekurzivno poziva ista funkcija
--ukoliko je ocena uneta van opsega, ponovo se zahteva unos ocene
--ukoliko je uneta ocena dobra menja se difoltna ocena u unetu
--ukoliko ocena nije unesena (== 0) ostaje difoltna ocena
--note: svaka figura ima svoju ocenu koja ce biti peti element torke
izaberiFiguru :: Int -> Int -> (String, String, String, Int)

