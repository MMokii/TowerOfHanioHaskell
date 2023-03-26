module Hanoi where

import Data.Char
import System.IO

{-prikaz igre kako funckionira
*Hanoi> igraj
Unesite broj tornjeva: 
3
Unesite broj diskova: 
3
[[1,2,3],[],[]]
Unesite disk u koji želite prebaciti: 
1
Unesite u koji toranj želite prebaciti disk: 
3
[[2,3],[],[1]]
Unesite disk u koji želite prebaciti: 
1
Unesite u koji toranj želite prebaciti disk: 
2
[[3],[2],[1]]
Unesite disk u koji želite prebaciti: 
3
Unesite u koji toranj želite prebaciti disk: 
2
[[3],[1,2],[]]
Unesite disk u koji želite prebaciti: 
1
Unesite u koji toranj želite prebaciti disk: 
3
[[],[1,2],[3]]
Unesite disk u koji želite prebaciti: 
2
Unesite u koji toranj želite prebaciti disk: 
1
[[1],[2],[3]]
Unesite disk u koji želite prebaciti: 
2
Unesite u koji toranj želite prebaciti disk: 
3
[[1],[],[2,3]]
Unesite disk u koji želite prebaciti: 
1
Unesite u koji toranj želite prebaciti disk: 
3
[[],[],[1,2,3]]
Bravo uspjeli ste!
-}

-- pomocu te funckije zapocinjemo Tower of Hanio
igraj:: IO ()
igraj = do
            x<-pokreni
            igra x
            return ()

igra :: [[Int]] -> IO [[Int]]
igra xs= if all null (init xs) then
                     do
                        prikazi xs
                        putStrLn("Bravo uspjeli ste!")
                        return xs
                        else
                            do
                                prikazi xs
                                putStrLn("Unesite disk u koji želite prebaciti: ")
                                x<-getLine
                                putStrLn("Unesite u koji toranj želite prebaciti disk: ")
                                y<-getLine
                                if provjera(xs,(read x::Int),(read y::Int)) then
                                  igra (pomaknidisk (xs, (read x::Int),(read y::Int)))
                                else
                                    do
                                    putStrLn("Pogrešan broj diska ili tornja.")
                                    igra xs

-- funckija koja nam prikazuje stanje tornjeva i stanje u njima tj. gdje su koji diskovi
prikazi::[[Int]]->IO ()
prikazi xs=putStrLn (show xs)

-- stvaramo tornjeve koji su ispunjeni sa diskovima, koliko diskova unsemo uvijek će biti upisani u prvi toranj (nulto mjesto)
kreiraj :: (Eq t1, Num t1, Num t2, Enum t2) => t1 -> t2 -> [[t2]]
kreiraj 1 _ = [[]]
kreiraj x y = [[1..y] ] ++ kreiraj (x-1) 0

-- pomaknidisk :: funkcija koja pomiče diskove z jednog tornja na drugi toranj

pomaknidisk :: ([[a]], Int, Int) -> [[a]]
pomaknidisk (xs,a,b) =if (a<b) then
        take (a-1) xs ++ [tail (xs!!(a-1))] ++drop a(take (b-1) xs)++ [(head (xs!!(a-1)) ):(xs!!(b-1))] ++drop b xs
                else
                take (b-1)xs++[(head (xs!!(a-1)) ):(xs!!(b-1))]++drop b(take (a-1) xs)++[tail (xs!!(a-1))]++drop a xs

-- provjera :: provjerava dali je pomak diskova moguč

provjera :: Ord a => ([[a]], Int, Int) -> Bool
provjera (xs, a, b)
  | (a>length xs || b>length xs) = False
  | (xs!!(a-1))==[] = False
  | (xs!!(b-1))==[] = True
  | head(xs!!(a-1))< head (xs!!(b-1)) = True
  | otherwise = False
-- pokreni :: kreiramo broj tornjeva i diskova i pomocu kreiraj funckije nacrtamo broj tornjeva i diska u tornju

pokreni :: IO [[Int]]
pokreni = do
            putStrLn("Unesite broj tornjeva: ")
            x<-getLine
            putStrLn("Unesite broj diskova: ")
            y<-getLine
            return (kreiraj (read x::Int) (read y::Int))





