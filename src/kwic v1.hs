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
type String = [Char] -- innecesario, es para beneficio del lector

-- Agrupar las palabras presentes en el título de una obra bibliográfica
-- Notar que solo se desechan los caracteres en blanco.
-- Los signos de puntuación se mantienen a la derecha de la palabra que les precede, si no hay espacios entre esos caracteres.

toWords :: [Char] -> [[Char]] -- String -> [String]
toWords [] = []
toWords (x:xs) | x == ' '  = toWords (dropWhile (' ' ==) xs)
               | otherwise = (x:takeWhile (' ' /=) xs) : toWords (dropWhile (' ' /=) xs)

{-
Articles:
  https://www.grammarly.com/blog/articles/
-}               

articles = ["a", "an", "some", "the"]

{- 
Prepositions:
  https://www.englishclub.com/grammar/prepositions-list.htm
  https://en.wikipedia.org/wiki/List_of_English_prepositions
  https://www.englishpage.com/prepositions/prepositions_list.htm
-}

-- incompleta: 
prepositions = toWords "aboard about above across after against along amid among anti around as at before behind below beneath beside besides between beyond but by concerning considering despite down during except excepting excluding following for from in inside into like minus near of off on onto opposite outside over past per plus regarding round save since than through to toward towards under underneath unlike until up upon versus via with within without"

{- 
Adjectives:
  https://grammar.yourdictionary.com/parts-of-speech/adjectives/list-of-adjective-words.html
  https://www.paperrater.com/page/lists-of-adjectives
  https://www.enchantedlearning.com/wordlist/adjectives.shtml
  https://patternbasedwriting.com/elementary_writing_success/list-4800-adjectives/
  https://www.talkenglish.com/vocabulary/top-500-adjectives.aspx
-}

-- incompleta: 
adjectives = toWords "fine beautiful peaceful"

{-
Adverbs:
  https://www.enchantedlearning.com/wordlist/adverbs.shtml
  https://grammar.yourdictionary.com/parts-of-speech/adverbs/list-of-100-adverbs.html
  https://7esl.com/list-of-adverbs/
  https://www.talkenglish.com/vocabulary/top-250-adverbs.aspx
  https://usefulenglish.ru/writing/list-of-adverbs
-}

-- incompleta:
adverbs = toWords "generally generously gently genuinely girlishly gladly gleefully gracefully graciously gradually gratefully greatly greedily grimly grudgingly"

{-
Pronouns:
  https://www.thefreedictionary.com/List-of-pronouns.htm
  https://www.english-grammar-revolution.com/list-of-pronouns.html
  https://www.really-learn-english.com/list-of-pronouns.html
  https://www.ef.com/ca/english-resources/english-grammar/pronouns/
  https://www.englishclub.com/vocabulary/pronouns-list.php
  https://www.englishclub.com/vocabulary/pronouns-type.php
-}

-- bastante completa:
pronouns = toWords "all another any anybody anyone anything aught both each each either enough everybody everyone everything few he her hers herself him himself his I it itself many me mine most myself naught neither no nobody none nothing nought one other others ought ours ourself ourselves several she some somebody someone something such suchlike that thee theirs theirself theirselves them themself themselves there these they thine this those thou thy thyself us we what whatever whatsoever whether which whichever whichsoever who whoever whom whomever whomso whomsoever whose whosever whosesoever whoso whosoever ye yon you yours yourself yourselves"


{-
Conjunctions:
  https://www.englishclub.com/grammar/conjunctions.htm
  https://www.grammarly.com/blog/conjunctions/
  https://dictionary.cambridge.org/grammar/british-grammar/conjunctions
  https://www.smart-words.org/linking-words/conjunctions.html
-}

-- algo básico, completar
conjunctions = toWords "after also although and as because before but either for how if neither nor once only or since so than that though till until when where whereas whether while yet"

-- la lista de palabras no significativas en los títulos puede parecer arbitraria, pero es conveniente 
notSignificants = articles ++ prepositions ++ adjectives ++ adverbs ++ pronouns ++ conjunctions

-- rotations recibe un título, como lista de palabras, y produce todas las rotaciones, sin importar si hay palabras no significativas
rotations xs = [ drop i xs ++ take i xs | i <- [0 .. n] ]
               where n = (length xs) - 1

-- Auxiliares para convertir palabras a minúsculas
lowercase = map toLower
lowercases = map lowercase

-- sigRotations recibe un título, como lista de palabras, y produce todas las rotaciones que comiencen con palabras significativas
sigRotations xs = [ drop i xs ++ take i xs | i <- [0 .. n], not ((lowercase (xs!!i)) `elem` notSignificants) ]
                  where n = (length xs) - 1

-- titSigRotations es como sigRotations, pero pone primero el título de la obra, antes de todas las rotaciones significativas
titSigRotations xs = xs : [ drop i xs ++ take i xs | i <- [0 .. n], not ((lowercase (xs!!i)) `elem` notSignificants) ]
                   where n = (length xs) - 1

-- putSpaces coloca un espacio en blanco entre las palabras presentes en una lista de palabras, y devuelve una hilera
putSpaces xss = tail (concat (map (' ':) xss))

-- sep pone una secuencia de caracteres, "><", para indicar que los caracteres a la izquierda de ">" están
-- al final del título original, mientras que los caracteres a la derecha de ">" están al inicio del título original
sep xs = init xs ++ [last xs ++ " ><"]

kwic = nub . sort . concat . map pre
       where pre ys = map putSpaces (sigRotations (sep (toWords ys)))

kwicTitles = nub . sort . concat . map pre
             where pre ys = map putSpaces (titSigRotations (sep (toWords ys)))

-- sequence_ tomada de Hudak, Peterson, Fasel. A Gentle Introduction to Haskell 98
{- sequence_ = foldr (>>) (return ())

sequence_ []     = return ()
sequence_ (a:as) = do a
                      sequence as
-}

-- Intentos para ver cómo sacar cada rotación en una línea aparte...
-- printKwic ts = [putStrLn t | t <- kwic ts]  -- esta funciona
printKwic ts = map putStrLn (kwic ts) -- esta también funciona (es lo mismo...)

printKwicTitles ts = map putStrLn (kwicTitles ts)

-- main = sequence_ (printKwic titles) -- titles está alambrado
main = sequence_ (printKwic titles) -- titles está alambrado

-- PRUEBAS
-- Títulos, para probar
-- titles = ["As falls Wichita, so falls Wichita Falls", "The Yellow Submarine", "Kind of Blue", "The Mythical Man-Month"]
titles = ["Descent of Man", "The Ascent of Man", "The Old Man and The Sea", "A Portrait of The Artist As a Young Man"]
