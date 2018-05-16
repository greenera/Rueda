module Figure
( Korak(..),
dodajFiguru,
izbrisiFiguru,
ucitajFigure,
izlistajFigure,
generisiListuZelja,
izaberiFiguru
)
where

-- NOTE: koordinate su podlezne izmenama (sigurno ce ostati kao polje u tipu Korak, ali ce se tip Koordinata verovatno menjati)

data Korak = Korak { formacija :: Formacija
		   , stav :: Stav
		   , naziv :: Naziv
		   , trajanje :: Trajanje
		   , nivo :: Nivo
		   , prioritet :: Prioritet
		   , zadrzavanje :: Zadrzavanje
		   , ocena :: Ocena 
		   , koordinate :: [(Tacka, Tacka, Tacka, Tacka)] -- prve dve su centar i orjentacija levog stopala, a druge dve desnog
		   , slika :: [(Putanja, Putanja)] --niz slika levog i desnog stopala duzine "trajanje"
		   , opis :: Opis -- encufla ili otvaranje
		   } deriving (Show, Eq)

data Formacija = Par | Linija | Rueda deriving (Show, Eq)
--Rueda = Rueda + Par
--Linija = Linija
--Par = Echo + Casino

data Stav = Echo | Casino | Dodatak deriving (Show, Eq)
data Tacka = Tacka Float Float deriving (Show, Eq)
type Naziv = String
type Nivo = Int
type Trajanje = Int
type Prioritet = Int
type Zadrzavanje = Int
type Ocena = Float -- dobija se primenom funkcije
type Putanja = String
type Opis = String
-- koordinate [(Tacka, Tacka, Tacka, Tacka)] -> prve dve su centar i orjentacija levog stopala, a druge dve desnog 
--koordinate podlozne menjanju

substring :: String -> String -> Bool
substring (x:xs) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

trim :: [Char] -> [Char]
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail :: [Char] -> [Char] -> [Char]
dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs

findIndexR :: Eq a => a -> [a] -> Int
findIndexR needle haystack =
    last $ findIndices (== needle) haystack

--ulaz: putanja do .txt fajla u kome se nalazi spisak figura sa potrebnim informacijama
--izlaz: niz IO figura (torki) (spadaju u IO jer se obavlja obrada ulaza tj fajla unutar funkcije)
-- tako da svakom polju torke odgovara po jedna informacija iz fajla 
--svaka linija sadrzi niz informacija o jednoj figuri, a informacije u fajlu su razdvojene dvotackom
--komentari su u fajlu obelezeni sa "--" na pocetku i kraju
--komentari i nepravilno popunjeni redovi se ignorisu
--note: funkcija ce biti sklona dopuni kako bude rastao br. informacija o figurama
ucitajFigure :: String -> IO [(String, String, String, Int)]
ucitajFigure imeFajla  = do
    linije <- fmap lines $ readFile imeFajla
    let ispravne = filter (/="") (drop ((findIndexR "--" linije)+1) linije)
    return (map ucitajFiguru ispravne)

-- ulaz:jedna linija iz fajla koja se parsira u jednu figuru
-- izlaz: parsirana figura
-- funkcija izdvojena posebno radi lakse promene nacina parsiranja linija u fajlu
ucitajFiguru :: String -> (String, String, String, Int)
ucitajFiguru linijaFajla =
    (
        trim $ delovi !! 0,
        trim $ delovi !! 1, 
        trim $ delovi !! 2, 
        read (delovi !! 3) ::Int
    )
    where delovi = splitOn ":" linijaFajla

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

              
--izlaz: niz odabranih figura
--opis:
--korisniku se daje da bira koje figure zeli da vezba i na osnovu toga se generise lista
--prvo korisnik treba da izabere tip figura [izaberiTip]
--zatim mu se ispisuju imenima figura sa sve rednim brojem [izlistajFigure]
--na kraju korisnik bira redne brojeve figura koje zeli, posle svakog rednog broja navodi koliko cesto na skali od 1 do 11 zeli da se proziva odabrana figura zavrsavajuci niz nulom [izaberiFiguru]
-- generisiListuZelja :: [(String, String, String, Int)]




--Ivana d'uradi nesto
provera :: (String, String, String, Int) -> Bool
provera korak = True

--ulaz: (tip figure, naziv figure, nazivFajla)
--izlaz: True/False (u zavisnosti da li je figura uspesno obrisana)
izbrisiFiguru :: (String, String) -> String -> IO Bool
izbrisiFiguru (nacin, ime) imeFajla = do
        fileExists <- doesFileExist imeFajla
        if fileExists  
            then do
                sadrzajL <- fmap lines (readFile imeFajla)
                let value = findIndex provera sadrzajL
                    noviSadrzaj = (case value of
                     Nothing -> ""
                     (Just br) -> unlines $ delete (sadrzajL !! br) sadrzajL)
                if ""==noviSadrzaj
                   then do
                       putStrLn "Korak ne postoji!"
                       return False
                   else do
                       removeFile imeFajla
                       writeFile imeFajla noviSadrzaj
                       return True
            else do 
                putStrLn "Fajl ne postoji!"
                return False
        where 
              provera = \x -> (nacin `substring` x) && (ime `substring` x)


--ulaz: figura za dodavanje
--izlaz: True/False (u zavisnosti da li je uspesno dodata)
--opis:
--fja dopusta unos jedino ispravnih vrednosti za prvi i drugi element torke:
-- linija (echo || ""), echo , casino (iz encufle || otvaranja), rueda (echo || casino)
--fja vrsi proveru: da li vec postoji figura sa istim 1. i 3. elementom
--	da li vec ima element u liniji za echo stav ukoliko se nova figura ubacuje u echo (i obrnuto)
--	da li je trajanje figure deljivo sa 8
--ukoliko su svi uslovi zadovoljeni figura se upisuje u txt fajl sa figurama i dodaje se ucitanom nizu figura
dodajFiguru :: (String, String, String, Int) -> String -> IO Bool
dodajFiguru korak@(nacin, stav, ime, trajanje) imeFajla = do
    fileExists <- doesFileExist imeFajla    
    if fileExists then do {
        if provera korak then do
            let string = nacin++" : "++stav++" : "++ime++" : "++(show trajanje)
            appendFile imeFajla $ string++"\n"
            return True
        else do return False
    }
    else do
        putStrLn "The file doesn't exist!"
        return False

--ulaz: redniBrojFigure, ocena od 1 do 11
--izlaz: korak sa zadatim rednim brojem
--opis:
--ukoliko je uneti redni broj 0 zavrsava se sa unosom
-- sta vraca funkcija u tom slucaju?
--ukoloko nije proverava se ima li figure sa tim rendim brojem
--ako ima vraca se ta figura, 
--inace se rekurzivno poziva ista funkcija
--ukoliko je ocena uneta van opsega, ponovo se zahteva unos ocene
--ukoliko je uneta ocena dobra menja se difoltna ocena u unetu
--ukoliko ocena nije unesena (== 0) ostaje difoltna ocena
--note: svaka figura ima svoju ocenu koja ce biti peti element torke
 --izaberiFiguru :: Int -> Int -> (String, String, String, Int)
 --izaberiFiguru 0 korak = ("", "", "", 0)
-- izaberiFiguru redniBrFig ocena =
--     if ocena < 1 || ocena > 11 then
--         putStrLn "Unesite ocenu iz opsega od 1 do 11!"

