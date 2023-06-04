module Artist where

import UdGraphic
import Test.QuickCheck
import Debug.Trace

-- Problema 1
-- Separa agafa una comanda,la separa i la retorna en una llista
separa :: Comanda -> [Comanda]
separa (Avança x) = [Avança x]
separa (Gira x) = [Gira x]
separa (c1 :#: c2) = separa c1 ++ separa c2
separa Para = []

-- Problema 2
-- Ajunta uneix multiples comandes d'una llista a una sola comanda
ajunta :: [Comanda] -> Comanda
ajunta x =  foldr1 (\a b -> a :#: b) x

-- Problema 3
-- Compara si dos comandes son equivalents mijtançant l'us de separa
prop_equivalent :: Comanda -> Comanda -> Bool
prop_equivalent c1 c2 = separa c1 == separa c2

-- Compara si dos comandes son equivalents mijtançant l'us de separa i ajunta
prop_split_join :: Comanda -> Comanda -> Bool
prop_split_join c1 c2 = ajunta(separa c1) == ajunta(separa c2)

-- Comprova que una llista de comandes no tingui ni Para ni :#:
prop_split :: [Comanda] -> Bool
prop_split [] = True 
prop_split((_ :#: _):xs) = False
prop_split (x:xs)
     |x == Para = False
     |otherwise = prop_split xs 


-- Problema 4
-- Donat un numero n i una comanda, genera una nova comanda amb el nombre n de copies de la comanda
copia :: Int -> Comanda -> Comanda
copia x c 
    | x > 1 = c :#: copia a c
    | x == 1 = c
    where a = x-1

-- Problema 5
-- Donat la distancia d'un costat, genera la Comanda un pentagon 
pentagon :: Distancia -> Comanda
pentagon x = copia 5 (Avança x :#: Gira 72)

-- Problema 6
-- Donat la distancia d'un costat, el nombre de cares i l'angle, genera la comanda d'un poligon
poligon :: Distancia -> Int -> Angle -> Comanda
poligon d c a = copia c (Avança d :#: Gira a)

-- Comprova si un poligon i un pentagon son equivalents
prop_poligon_pentagon :: Comanda -> Comanda -> Bool
prop_poligon_pentagon c1 c2 = c1 == c2

--prop_poligon_pentagon :: (Distancia -> Int -> Angle -> Comanda) -> Distancia -> Int -> Angle -> (Distancia -> Comanda) -> Distancia -> Bool
--prop_poligon_pentagon f1 c1 c11 c111 f2 c2 = f1 c1 c11 c111 == f2 c2

-- Problema 7
-- Donat la distancia d'un costat, les linies que s'han de dibuixar, a quantitat per la qual la longitud dels segments successius canvia, i l'angle, genera la Comanda d'un espiral
espiral :: Distancia -> Int -> Distancia -> Angle -> Comanda
espiral dis cos pas ang 
    |cos == 1 = (Avança dis :#: Gira ang)
    |cos > 1 = (Avança dis :#: Gira ang) :#: espiral dista cos2 pas ang
    where dista = dis+pas
          cos2 = cos-1
  
-- Problema 9
-- Donada una comanda p, retorna una comanda q que dibuixa la mateixa imatge, pero te les seguents propietats:
-- • q no conte les comandes Para, Avança 0 o Gira 0, llevat que la comanda sigui equivalent a Para.
-- • q no conte comandes Avança consecutives.
-- • q no conte comandes Gira consecutives.
optimitza :: Comanda -> Comanda
optimitza c = ajunta(sumaAvança(separa c) 0)

-- Suma totes les comandes Avança consecutives
sumaAvança :: [Comanda] -> Distancia -> [Comanda]
sumaAvança [] d = if d == 0 then [] else [Avança d]
sumaAvança (Avança x:xs) d = sumaAvança xs (x + d) 
sumaAvança (Gira x:xs) d 
          |x == 0 = sumaAvança xs d
          |x > 0 = if d==0 then sumaGira xs d else [Avança d] ++ sumaGira xs x 

-- Suma totes les comandes Gira consecutives
sumaGira :: [Comanda] -> Angle -> [Comanda]
sumaGira [] d = if d == 0 then [] else [Gira d]
sumaGira (Gira x:xs) d = sumaGira xs (x + d) 
sumaGira (Avança x:xs) d 
         |x == 0 = sumaGira xs d
         |x >0 = if d==0 then sumaAvança xs d else [Gira d] ++ sumaAvança xs x 

-- Problema 10
-- Donat un int n, reescriu n vegades "f" per "f+f-f-f+f", i despres reescriu f per Avança 1, + per Gira 90, i - per Gira -90
triangle :: Int -> Comanda
triangle n = reescriptura n "+f"

-- Reescriu n vegades "f", i despres genera la Comanda
reescriptura :: Int -> String -> Comanda
reescriptura 0 p = ajunta (converteix p)
reescriptura n p = reescriptura (n-1) (reemplacar p)

-- Reescriu f  per "f+f-f-f+f"
reemplacar :: String -> String
reemplacar [] = []
reemplacar (x:xs) 
   |x == 'f' = "f+f-f-f+f" ++ reemplacar xs 
   |otherwise = x : reemplacar xs

-- Converteix 'f',+ i -
converteix :: String -> [Comanda]
converteix [x] 
   |x == '+' = [Gira 90] 
   |x == '-' = [Gira (-90)] 
   |x == 'f' = [Avança 1]
converteix(x:xs)
   |x == '+' = [Gira 90] ++ converteix xs
   |x == '-' = [Gira (-90)] ++ converteix xs
   |x == 'f' = [Avança 1] ++ converteix xs
   |otherwise = converteix xs


-- Problema 11

fulla :: Int -> Comanda
fulla = undefined

-- Problema 12
-- Donat un int n, reescriu n vegades "l" per "+rf-lfl-fr+"
hilbert :: Int -> Comanda
hilbert n = reescripturaHilbert n "l"

-- Reescriu n vegades "l", i despres genera la Comanda
reescripturaHilbert :: Int -> String -> Comanda
reescripturaHilbert 0 p = converteixHilbert p
reescripturaHilbert n p = reescripturaHilbert (n-1) (reemplacarHilbert p)

-- Reescriu l  per "+rf-lfl-fr+" i r per "-lf+rfr+fl"
reemplacarHilbert :: String -> String
reemplacarHilbert [] = []
reemplacarHilbert (x:xs) 
   |x == 'l' = "+rf-lfl-fr+" ++ reemplacarHilbert xs 
   |x == 'r' = "-lf+rfr+fl" ++ reemplacarHilbert xs 
   |otherwise = x : reemplacarHilbert xs

-- Converteix 'f',+ i -
converteixHilbert :: String -> Comanda
converteixHilbert [] = Para
converteixHilbert(x:xs)
   |x == '+' = Gira (-90) :#: converteixHilbert xs
   |x == '-' = Gira 90 :#: converteixHilbert xs
   |x == 'f' = Avança 1 :#: converteixHilbert xs
   |otherwise = converteixHilbert xs

-- Problema 13
-- Donat un int n, reescriu n vegades "f" per "g+f+g"
fletxa :: Int -> Comanda
fletxa n= reescripturaFletxa n "f"

-- Reescriu n vegades "f", i despres genera la Comanda
reescripturaFletxa :: Int -> String -> Comanda
reescripturaFletxa 0 p = ajunta (converteixFletxa p)
reescripturaFletxa n p = reescripturaFletxa (n-1) (reemplacarFletxa p)

-- Reescriu f  per "g+f+g" i g per "f-g-f"
reemplacarFletxa :: String -> String
reemplacarFletxa [] = []
reemplacarFletxa (x:xs) 
   |x == 'f' = "g+f+g" ++ reemplacarFletxa xs 
   |x == 'g' = "f-g-f"
   |otherwise = x : reemplacarFletxa xs

-- Converteix 'f',+ i -
converteixFletxa :: String -> [Comanda]
converteixFletxa [x] 
   |x == '+' = [Gira 60] 
   |x == '-' = [Gira (-60)] 
   |x == 'f' = [Avança 1]
converteixFletxa(x:xs)
   |x == '+' = [Gira 60] ++ converteixFletxa xs
   |x == '-' = [Gira (-60)] ++ converteixFletxa xs
   |x == 'f' = [Avança 1] ++ converteixFletxa xs
   |otherwise = converteixFletxa xs

-- Problema 14

branca :: Int -> Comanda
branca = undefined 