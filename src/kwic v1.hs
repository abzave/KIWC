-- KWIC - Key word in context
-- Autor: ITZ
-- Un hack rápido, basado en mi propio código de 1989.11

{- 
Referencias:
  https://www.cs.cmu.edu/~ModProb/KWIC.html
  https://www.librarianshipstudies.com/2017/02/keyword-in-context-kwic-indexing.html
  https://en.wikipedia.org/wiki/Key_Word_in_Context
-}

import Data.Char -- importar algunas funciones sobre datos de tipo Char
import Data.List -- acceso a funciones como sort y nub (remdups)
import System.Directory
import Control.Monad
type String = [Char] -- innecesario, es para beneficio del lector

-- Agrupar las palabras presentes en el título de una obra bibliográfica
-- Notar que solo se desechan los caracteres en blanco.
-- Los signos de puntuación se mantienen a la derecha de la palabra que les precede, si no hay espacios entre esos caracteres.

toWords :: [Char] -> [[Char]] -- String -> [String]
toWords [] = []
toWords (x:xs) | x == ' '  = toWords (dropWhile ('\n' ==) xs)
               | otherwise = (x:takeWhile ('\n' /=) xs) : toWords (dropWhile ('\n' /=) xs)

-- rotations recibe un título, como lista de palabras, y produce todas las rotaciones, sin importar si hay palabras no significativas
rotations xs = [ drop i xs ++ take i xs | i <- [0 .. n] ]
               where n = (length xs) - 1

-- Auxiliares para convertir palabras a minúsculas
lowercase = map toLower
lowercases = map lowercase

-- sigRotations recibe un título, como lista de palabras, y produce todas las rotaciones que comiencen con palabras significativas
sigRotations xs notSignificants = [ drop i xs ++ take i xs | i <- [0 .. n], not ((lowercase (xs!!i)) `elem` notSignificants) ]
                  where n = (length xs) - 1

-- titSigRotations es como sigRotations, pero pone primero el título de la obra, antes de todas las rotaciones significativas
titSigRotations xs notSignificants = xs : [ drop i xs ++ take i xs | i <- [0 .. n], not ((lowercase (xs!!i)) `elem` notSignificants) ]
                   where n = (length xs) - 1

-- putSpaces coloca un espacio en blanco entre las palabras presentes en una lista de palabras, y devuelve una hilera
putSpaces xss = tail (concat (map (' ':) xss))

-- sep pone una secuencia de caracteres, "><", para indicar que los caracteres a la izquierda de ">" están
-- al final del título original, mientras que los caracteres a la derecha de ">" están al inicio del título original
sep xs = init xs ++ [last xs ++ " ><"]

kwic notSignificants = nub . sort . concat . map pre
       where pre ys = map putSpaces (sigRotations (sep (words ys)) notSignificants)

kwicTitles notSignificants = nub . sort . concat . map pre
             where pre ys = map putSpaces (titSigRotations (sep (words ys)) notSignificants)

-- sequence_ tomada de Hudak, Peterson, Fasel. A Gentle Introduction to Haskell 98
{- sequence_ = foldr (>>) (return ())

sequence_ []     = return ()
sequence_ (a:as) = do a
                      sequence as
-}

-- Intentos para ver cómo sacar cada rotación en una línea aparte...
-- printKwic ts = [putStrLn t | t <- kwic ts]  -- esta funciona
printKwic ts ns = map putStrLn (kwic ns ts) -- esta también funciona (es lo mismo...)

printKwicTitles ts ns = map putStrLn (kwicTitles ns ts)

askFileName message = do
    putStrLn message
    getLine

getExistingFile message = do 
  input <- askFileName message
  fileExists <- doesFileExist input
  if fileExists then return input else (getExistingFile message)

getNonExistingFile message = do 
  input <- askFileName message
  fileExists <- doesFileExist input
  if fileExists then (getNonExistingFile message) else return input

main = do
  titlesFile <- getExistingFile "Ingrese el nombre del archivo con los titulos:"
  wordsFile <- getExistingFile "Ingrese el nombre del archivo con las palabras no significativas:"
  outputFile <- getNonExistingFile "Ingrese el nombre del archivo donde se espera la salida:"
  titlesList <- readFile titlesFile
  wordsList <- readFile wordsFile
  sequence_ (printKwic (toWords titlesList) (words wordsList)) 

