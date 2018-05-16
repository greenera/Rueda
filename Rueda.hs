import Data.List
import Data.List.Split
import Data.Char
import System.Directory
import System.Process

import Figure


-- NOTE: koordinate su podlezne izmenama (sigurno ce ostati kao polje u tipu Korak, ali ce se tip Koordinata verovatno menjati)

--ulaz: niz koraka
--izlaz : informacija je li uspesno napravljena plejlista
--opis:
--fja pravi txt datoteku sa odabranom listom koraka
zapamtiPlejlistu :: [Korak] -> Bool

--ulaz: svi dostupni koraci (iz plejliste ili figure.txt fajla)
--izlaz: niz koraka od kojih se pravi plejlista
--opis: 
--f-jom se biraju koraci za plejlistu pod jednim pravilom da nema ponavljanja istog koraka vise puta
odaberiZaPlejlistu :: [Korak] -> [Korak]

--ulaz: svi dostupni koraci (iz druge plejliste ili figure.txt fajla)
--izlaz: informacija je li uspesno napravljena plejlista
kreirajPlejlistu [Korak] -> Bool
kreirajPlejlistu x = zapamtiPlejlistu . odaberiPlejlist x


--ulaz: niz koraka od kojih se biraju koraci za animaciju
--izlaz: koraci za animaciju
--opis:
--odabir se vrsi tako sto korisnik prvo izabere formaciju (svi moraju biti iste formacije)
--echo je automatski ubacen u plejlistu
--zatim moze izabrati nivo do kog se ubacuju koraci
--na kraju korisnik sa plus i minus upravlja koji korak zeli da ubaci a koji da izbaci iz liste
--sve vreme potrebno je korisniku lepo prikazati korake koji su na raspolaganju i koji su ubaceni u listu za reprodukovanje
odaberiZaAnimaciju :: [Korak] -> String -> [Korak]

--ulaz: Naziv pesme
--izlaz: da li je nadjena pesma
--opis: prima naziv pesme i javlja da li je pronadjena apsolutna putanja
izaberiPesmu :: String -> Either Bool String




-- ulaz: putanja do fajla koji treba pustiti
-- izlaz: Bool koji govori da li je funkcija uspesno odsvirala svoje
-- NAPOMENA: neophodno skinuti ffmpeg za pustanje mp3 fajlova
-- (sudo apt-get install ffmpeg)
-- TODO: dodati Assert-ove tj igrati se sa ffplay u terminalu da bi se naslo 
-- pravilo za parsiranje ispisa na stderr (greska)
-- ffplay -autoexit -vn -nodisp muzika/test.mp3
-- "seq" ["1", "10"] ""
pustiMuziku putanja = do
    povVr@(exitCode, output, greska) <- readProcessWithExitCode "ffplay" ["-autoexit", "-vn", "-nodisp", putanja] ""
    print povVr

--Ulaz: True/False (u zavisnosti da li je pustena muzika)
--Izlaz: (Interval izmedju dva takta, vreme poslednjeg otkucaja + izracunati interval)
--Opis:
--funkcija reaguje na enter taster
--racuna artmeticku vrednost izmedju cetiri otkucaja i vraca kao prvi argument taj interval, a kao drugi vreme poslednjeg otkucaja sabrano sa intervalom
--funkcija iziskuje minimum 4 otkucaja, a ukoliko je vise puta pritisnut enter racuna samo poslednja cetiri otkucaja
--ukoliko posle otkucaja ne sledi naredni otkucaj u roku od 5 sekundi a nema dovoljno (4) otkucaja funkcija signalizira gresku (vraca -1,-1)
--uhvatiRitam :: Bool -> (Time, Time)

