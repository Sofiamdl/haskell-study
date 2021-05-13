import Data.Char ( ord )
import System.IO ()
answer :: Int
answer = 42

imc :: Double -> Double -> Double
imc p h = p / (h *h)

maiorQue2 :: Int -> Int -> Int -> Bool
maiorQue2 a b c = (a <= b) && (b <= c)

vendas :: Int -> Int
vendas v = 12 + mod (v*2) 3

qntdSema :: Int -> Int -> Int
qntdSema s n | n < 0 = 0
             | vendas n == s = 1 + qntdSema s (n-1)
             | otherwise = qntdSema s (n-1)

vendasSemanas :: Int -> Int
vendasSemanas n | n < 0 = 0
                | otherwise = vendas n + vendasSemanas (n-1) 

ehPrimoAux :: Int -> Int -> Bool
ehPrimoAux n s | s == 0 || s == 1 = True
               | mod n s == 0 = False
               | otherwise = ehPrimoAux n (s-1) 

ehPrimo :: Int -> Bool
ehPrimo n | n == 0 || n == 1 = False
          | otherwise = ehPrimoAux n (n-1)

primosEntreSi :: Int -> Int -> Bool
primosEntreSi numUm numDois | numUm == 0 || numDois == 0 = False
                            | otherwise = primosAux numUm numDois 2

primosAux :: Int -> Int -> Int -> Bool
primosAux numUm numDois divi | divi > numUm || divi > numDois               = True
                             | mod numUm divi == 0 && mod numDois divi == 0 = False
                             | otherwise = primosAux numUm numDois (divi+1)

fat :: Int -> Int
fat n | n == 0 = 1
      | otherwise = n * fat (n-1)

allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = allEqual a b c && (c == d)

equalCount :: Int -> Int -> Int -> Int
equalCount a b c | allEqual a b c = 3
                 | a == b || b == c || a == c = 2


-- AULA 2
addEspacos :: Int -> String
addEspacos n | n == 0 = ""
             | otherwise = " " ++ addEspacos (n-1)

paraDireita :: Int -> String -> String
paraDireita n string = addEspacos n ++ string

--media :: Float -> Float
media :: Fractional a => Int -> a
media l = fromIntegral(vendasSemanas l) / fromIntegral(l +1)

vendasTotais:: Int -> Int -> String
vendasTotais n aux | aux > n = "Total" ++ addEspacos 4 ++ show (vendasSemanas n) ++"\nMédia    " ++ show (media n)
                   | otherwise = addEspacos 2 ++ show aux ++ addEspacos 6 ++ show (vendas aux) ++ "\n" ++ vendasTotais n (aux+1)

totalVendas :: Int -> String
totalVendas n = "Semana  Venda\n" ++ vendasTotais n 0

--AULA 3
menorQue :: Ord a => [a] -> [a]
menorQue (a:as) | a : as == [] || as == [] = []
                | a > (head as) = (head as): menorQue(a:tail as)
                | otherwise = menorQue(a:tail as)

maiorQue :: Ord a => [a] -> [a]
maiorQue (a:as) | a : as == [] || as == []= []
                | a <= (head as) = (head as): maiorQue(a:tail as)
                | otherwise = maiorQue(a:tail as)

{-qs :: Ord a => [a] -> [a]
qs (a:as) | (a:as) == [] = []
          | otherwise = qs (menorQue (a:as)) ++ [a] ++ qs (maiorQue (a:as))-}

qs :: Ord a => [a] -> [a]
qs [] = []
qs (a:as) = qs (menorQue (a:as)) ++ [a] ++ qs (maiorQue (a:as))

paresFib :: (Eq t, Num t, Num a) => t -> a -> a -> [a]
paresFib 0 x y= []
paresFib n x y = (x + y): paresFib (n-1) (x+y+y) (x+y+y+x+y)

parFib :: (Eq t, Num t, Num a) => t -> [a]
parFib n = paresFib n 1 1

--Questão de ordenar a soma louca
listaNum :: Show a => [a] -> [String]
listaNum [] = []
listaNum (a:as) = show a : listaNum as

somarlista :: [Char] -> Int
somarlista [] = 0
somarlista (a:as) = (ord a) - 48 + somarlista as

listaSomNum :: [[Char]] -> [[Int]]
listaSomNum [] = []
listaSomNum (a:as) = [(somarlista a: [read a])] ++ listaSomNum as

menorQueSom :: Ord a => [[a]] -> [[a]]
menorQueSom (a:as) | a : as == [] || as == [] = []
                   | head a > (head (head as)) = (head as): menorQueSom(a:tail as)
                   | otherwise = menorQueSom(a:tail as)
            
maiorQueSom :: Ord a => [[a]] -> [[a]]
maiorQueSom (a:as) | a : as == [] || as == []= []
                   | head a <= (head (head as)) = (head as): maiorQueSom(a:tail as)
                   | otherwise = maiorQueSom(a:tail as)

qSoma :: Ord a => [[a]] -> [[a]]
qSoma [] = []
qSoma (a:as) = qSoma (menorQueSom (a:as)) ++ [a] ++ qSoma (maiorQueSom (a:as))

aux :: Show a => [a] -> [[Int]]
aux lista = qSoma(listaSomNum (listaNum lista))

ordenarAux :: [[a]] -> [a]
ordenarAux [] = []
ordenarAux (a:as) = head (tail a): ordenarAux as

ordenar :: Show a => [a] -> [Int]
ordenar lista = ordenarAux (aux lista)


--AULA 4

menorMaior :: Ord b => b -> b -> b -> (b, b)
menorMaior a b c = (compar a b c (>=), compar a b c (<=))

compar :: p -> p -> p -> (p -> p -> Bool) -> p
compar a b c comp | comp a b && comp b c = a
                  | comp b c && comp c a = b
                  | otherwise = c

menorMaior2 :: Int -> Int -> Int-> (Int,Int)
menorMaior2 a b c | maiorQue2 a b c = (a, c)
                  | maiorQue2 a c b = (a, b)
                  | maiorQue2 b c a = (b, a)
                  | maiorQue2 b a c = (b, c)
                  | maiorQue2 c a b = (c, b)
                  | otherwise = (c, a)


ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c) | maiorQue2 a b c = (a, b, c)
                       | maiorQue2 a c b = (a, c, b)
                       | maiorQue2 b c a = (b, c, a)
                       | maiorQue2 b a c = (b, a, c)
                       | maiorQue2 c a b = (c, a, b)
                       | otherwise = (c, b, a)

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

retornaPrim :: Reta -> Ponto
retornaPrim (a, b) = a

retornaSeg :: Reta -> Ponto
retornaSeg (a,b) = b

vertical :: Reta -> Bool
vertical a | f == z = True
           | otherwise = False
             where (f,b) = retornaPrim a
                   (z,x) = retornaSeg a
retornaX :: Ponto -> Float
retornaX (a,b) = a
retornaY :: Ponto -> Float
retornaY (a,b) = b

vertical2 :: Reta -> Bool
vertical2 (a, b) | retornaX a == retornaX b = True
                 | otherwise = False

pontoY :: Float -> Reta -> Float
pontoY num (a,b)=
      let m = (retornaY a - retornaY b) / (retornaX a - retornaX b)
      in m * (num - retornaX b) + retornaY b

--AULA 5
type Pessoa = String
type Livro = String
type Banco = [(Pessoa, Livro)]

bancoExemplo :: Banco
bancoExemplo = [("Sofia", "Harry Potter"), ("Rene", "Feios"), ("Graça", "Freud"), ("Sofia", "Jogos Vorazes")]

livros :: Banco -> Pessoa ->[Livro]
livros bd p= [l | (pp, l) <- bd, pp == p]

emprestimos :: Banco -> Livro -> [Pessoa]
emprestimos bd l = [p | (p, ll) <- bd, l == ll]

emprestado :: Banco -> Livro -> Bool
emprestado bd l = (emprestimos bd l) /= []

qtdEmprestimos :: Banco -> Pessoa -> Int
qtdEmprestimos bd p = length (livros bd p)

emprestar :: Banco -> Pessoa -> Livro -> Banco
emprestar [] p l = [(p,l)]
emprestar ((pp,ll):as) p l | pp == p && ll == l = ((pp,ll):as)
                           | otherwise = (pp,ll): emprestar as p l

devolver :: Banco -> Pessoa -> Livro -> Banco
devolver [] p l = [] 
devolver ((pp,ll):as) p l | pp == p && ll == l = (head as:tail as)
                          | otherwise = (pp,ll): emprestar as p l

devolverCompre ::  Banco -> Pessoa -> Livro -> Banco
devolverCompre bd p l = [(pessoa,livro) | (pessoa,livro) <- bd, (pessoa,livro) /= (p,l)]

qsFino :: Ord a => [a] -> [a]
qsFino [] = []
qsFino (a:as)  = qsFino ([x | x <- as , x <= a]) ++ [a] ++ qsFino([x | x <- as , x > a])

getword :: String -> String
getword "" = ""
getword (a:as) | a /= ' ' = a:(getword as)
               | otherwise = ""

dropword :: String -> String
dropword "" = ""
dropword (a:as) |  a /= ' ' = dropword as
                | otherwise = a:as
            
dropspace :: String -> String
dropspace "" = ""
dropspace (a:as) | a == ' ' = dropspace as
                 | otherwise = a:as

splitwords :: String -> [String]
splitwords [] = []
splitwords (a:as) | a /= ' ' = getword(a:as):splitwords (dropword(a:as))
                  | otherwise = splitwords (dropspace (a:as)) 
                  
{-Processamento de texto
type Line = [Word]
getLine :: Int -> [Word] -> Line
dropLine :: Int -> [Word] -> [Word]
splitLines :: [Word] -> [Line]
fill :: String -> [Line]
fill st = splitLines (splitWords st)
joinLines :: [Line] -> String-}

--AULA 6
{-agrupar :: Eq t => [[t]] -> [(t,Int)]
agrupar [] = []
agrupar (a:as) = (ind, num): agrupar novaLista
                  where (ind, num, novaLista) = agruparAux (a:as) (a, 0 , [])-}

juntarElemLista :: Eq t => [[t]] -> [t]
juntarElemLista [] = []
juntarElemLista (a:as) = a ++ juntarElemLista as

agruparAux ::  Eq a => [a] -> (a, Int, [a]) -> [(a, Int)]
agruparAux [] (ind,num,[])= [(ind,num)]
agruparAux [] (ind, num, listaAux) = (ind, num): agruparAux listaAux ((head listaAux),0, [])
agruparAux (a:as) (ind, num, b) | a == ind = agruparAux as (ind, num+1, b)
                                | otherwise = agruparAux as (ind, num, (b++[a]))
            
agrupar :: Eq t=> [[t]] -> [(t, Int)]
agrupar lista = agruparAux (a:as) (a, 0 ,[])
                              where (a:as) = juntarElemLista lista


-- AULA 9 + AULA 10
iscrescent :: (Int -> Int) -> Int -> Bool
iscrescent f num = length [False | a <- [0..num], a /= num && f a >= f (a+1)] == 0

fun :: Int -> Int
fun n =  n*n 

elevarQuadra :: [Int] ->[Int]
elevarQuadra lista = map fun lista

somadosQuadra :: [Int] -> Int
somadosQuadra lista = foldr (+) 0 (elevarQuadra lista)

maiorZero :: [Int] -> [Int]
maiorZero lista = filter ((<) (0)) lista

mapr :: (t -> u) -> [t] -> [u]
mapr a b = foldr (\x y-> (a x):y) [] b

{-
concatena :: (t -> a) -> t -> [a] -> [a]
concatena func a b = [func a] ++ b-}

filterR :: (t -> Bool) -> [t] -> [t]
filterR func lista = foldr (\x y-> [x |func x]++y) [] lista
{-
concatena2 :: (a -> Bool) -> a -> [a] -> [a]
concatena2 func a b = [a | func a] ++ b-}

maiores :: [[Int]] -> [Int]
maiores lista = foldr (\x y -> (foldr (max) 0 x):y) [] lista
{-
pegarmaiores1 :: [Int] ->[Int]->[Int]
pegarmaiores1 lista list= [foldr (max) 0 lista]++ list
-}
takewhile :: (t-> Bool) -> [t] ->[t]
takewhile func [] = []
takewhile func (a:as) | func a = a: takewhile func as
                      | otherwise = []

dropwhile :: (t-> Bool) -> [t] ->[t]
dropwhile _ [] = []
dropwhile func (a:as) | not (func a) = (a:as)
                      | otherwise = dropwhile func as
            
g:: (t-> u -> v) -> u-> t-> v
g f = \t u  -> f u t 

getWord2:: String -> String
getWord2 stri = takewhile(/= ' ') stri

dropWord2:: String->String
dropWord2 stri = dropwhile(/= ' ') stri

dropSpace2 :: String->String
dropSpace2 stri = dropwhile(== ' ') stri

{-AULA 11---(.)::(b->c)->(a->b)->(a->c)
dropSpace
= dropWhile (member whitespace)
dropwhile :: (t-> Bool) -> [t] ->[t]
member ::[t]->t->Bool
whitespace :: [Char]
member whitespace =  Char -> Bool
dropwhile (member whitespace) = [Char] -> [Char]


• dropWord
= dropWhile (not.(member whitespace))
dropwhile :: (t-> Bool) -> [t] ->[t]
member ::[t]->t->Bool
not :: Bool -> Bool
whitespace :: [Char]
member whitespace =  Char -> Bool
(.) :: (b -> c) -> (a -> b) -> a -> c
a = Char
b = Bool
b = Bool
c = Bool
(not.(member whitespace)) = Char -> Bool
dropWhile (not.(member whitespace)) = [Char] -> [Char]

• getWord
= takeWhile (not . member whitespace)
member ::[t]->t->Bool
not :: Bool -> Bool
takeWhile ::(a -> Bool) -> [a] -> [a]
whitespace :: [Char]
member whitespace =  Char -> Bool
(.) :: (b -> c) -> (a -> b) -> a -> c
a = [t]
b = t-> Bool
b = Bool
c = Bool
not. member = [t] -> t -> Bool
not.member whitespace =  Char -> Bool
takeWhile (not . member whitespace) = [Char] -> [Char]



• (+2)
2:: Num p => p
(+) Num => p -> p -> p -> p
(.)::(b->c)->(a->b)->(a->c)
Num=> p -> p -> p

• (>2)
2:: Num p => p (todos Num podem ter Ord ent ta dboa)
(+) Ord => p -> p -> p -> Bool
(.)::(b->c)->(a->b)->(a->c)
(Num p, Ord p) => p -> Bool

• (3:)
3:: Num p => p
(:) :: a -> [a] -> [a]
(.)::(b->c)->(a->b)->(a->c)
Num => a -> [a] -> [a]

• (++ “\n“)
"\n" :: [Char]
(++) :: [a] -> [a] ->[a]
(.)::(b->c)->(a->b)->(a->c)
[Char] -> [Char]

• filter (>0).map (+1)
map :: (a -> b) -> [a] -> [b]
filter :: (a -> Bool) -> [a] -> [a]
(.)::(b->c)->(a->b)->(a->c)

(+1):: Num=> p -> p -> p
(>0) :: (Num m, Ord m) => m -> Bool

map(+1):: Num => p -> [p] -> [p]
filter (>0):: (Ord m, Num m)=>  [m] -> [m]

a = Num=> p -> [p]
b = [p]
b = (Ord m, Num m)=>  [m]
c = [m]
p==m
resp = (Ord m, Num m)=>  [m] -> [m]




• double = map (*2)
map = (a -> b) -> [a] -> [b]
*2 = Num => x -> x -> x
Num => z -> [z] -> [z]

• dificil = map.filter
map :: (a-> b) -> [a] -> [b]
filter :: (x-> Bool) -> [x] -> [x]
(.)::(b->c)->(a->b)->(a->c)
a = (x-> Bool)
b = [x] -> [x]
b = (a-> b)
c = [a] -> [b]

(x -> Bool) -> ([[x]] -> [[x]])

• maisdificil = map.foldr
map :: (a-> b) -> [a] -> [b]
foldr :: Foldable => t -> (a -> v -> v) -> v -> t a -> v

(.)::(b->c)->(a->b)->(a->c)
a = Foldable => a -> (a -> v -> v)
b = v -> t a -> v
b = (a-> b)
c = [a] -> [b]
Foldable => t -> (a -> v -> v) -> [v] -> [t a -> v]

• maisainda = foldr.map
foldr :: Foldable => t -> (a -> v -> v) -> v -> t a -> v
map :: (a-> b) -> [a] -> [b]
a= (a -> b)
      error            b = [a] -> [b]
      error            b = Foldable => t -> (a -> v -> v)
c = v -> t a -> v
-}

--AULA 12
data Shape = Circle Float | Rectangle Float Float
area :: Shape -> Float
area (Circle raio) = raio*raio* pi
area (Rectangle dimen1 dimen2) = dimen1*dimen2

data Expr = Lit Int| Add Expr Expr| Sub Expr Expr
            deriving(Show)
eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

showExpr :: Expr -> String
showExpr (Lit n)= show n
showExpr (Add e1 e2) = showExpr  e1 ++ "+" ++ showExpr e2
showExpr (Sub e1 e2) = showExpr e1 ++ "-" ++ showExpr e2

data List l = Nil | Const l (List l)
            deriving(Show)

toList :: List t -> [t]
toList Nil = []
toList (Const a as) = a:toList as

fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = Const a (fromList as)

data Tree l = NilT | Node l (Tree l) (Tree l)
            deriving(Eq, Show, Ord)


depth :: Tree t -> Int
depth NilT = 0
depth (Node ele treesq tredir) = max ((depth treesq) + 1) ((depth tredir) + 1)

collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node ele treesq tredir) =  collapse treesq ++[ele]++ collapse tredir

addEle :: Ord t => Tree t -> t -> Tree t
addEle NilT ele = (Node ele NilT NilT)
addEle (Node qqr treesq tredir) ele | ele >= qqr = Node qqr treesq (addEle tredir ele)
                                    | otherwise = Node qqr (addEle treesq ele) tredir


mapTree :: (t -> u) -> Tree t -> Tree u
mapTree _ NilT = NilT
mapTree func (Node ele treesq tredir) = Node (func ele) (mapTree func treesq) (mapTree func tredir)


splitWordsCauda :: String -> [String]
splitWordsCauda string = swaux string "" []

swaux :: String -> String -> [String] -> [String]
swaux "" "" lista = lista
swaux "" aux lista = swaux "" "" (lista ++ [aux])
swaux (a:as) aux lista | a /= ' ' = swaux as (aux++[a]) lista
                       | otherwise = swaux as "" (lista++[aux])

shorten :: String -> String
shorten ytb = "http://youtu.be/" ++ urlytb ytb

urlytb :: String -> String
urlytb link = removerPalavra link "https://www.youtube.com/watch?v="

removerPalavra :: String -> String -> String
removerPalavra "" _ = ""
removerPalavra resto "" = resto
removerPalavra (a:as) (b:bs) | a == b = removerPalavra as bs 
                             | otherwise = as


diminuirLink :: String -> String -> IO ()
diminuirLink [] aux = putStrLn (shorten aux)
diminuirLink (a:as) aux | a == '\n' = do { 
                                         putStrLn (shorten aux);
                                         diminuirLink as ""}
                        | otherwise = diminuirLink as (aux++[a])


abrirArquivo :: FilePath -> IO ()
abrirArquivo arq = readFile arq >>= \x -> diminuirLink x ""

      
main:: IO()
main = abrirArquivo "C:\\Users\\rene_\\Downloads\\Projeto\\ytb.txt"


import Prelude hiding (Maybe (..))
{-Data;Tipo;Compra;Valor;
14 JAN;Amazon;40.32;
15 JAN;Uber;14.84;
25 JAN;Uber;34.24;
02 FEV;Spotify;8.50;
06 FEV;Uber;6.94;
05 MAR;Burger;29.90;
10 MAR;Burger;24.99;
15 MAR;UCI;19.00;
08 ABR;Itunes;3.50;
13 ABR;Picpay;20.00;
-}

{-Escreva uma função logMes :: String -> String -> Double que recebe uma String (JAN, FEV, MAR ou ABR), 
uma String referente a fatura anual e retorna o total gasto no mês em questão.

Exemplo:
Main> logMes "JAN" logCartao
89.4
-}
--Q1
logCartao :: [Char]
logCartao = "14 JAN;Amazon;40.32;15 JAN;Uber;14.84;25 JAN;Uber;34.24;02 FEV;Spotify;8.50;06 FEV;JAN;6.94;05 MAR;Burger;29.90;10 MAR;Burger;24.99;15 MAR;UCI;19.00;08 ABR;Itunes;3.50;13 ABR;Picpay;20.00;"

lista :: String -> String -> [String]
lista "" _ = []
lista (a:as) aux | a == ';' || a == ' ' = reverse(aux): lista (as) ""
                 | otherwise = lista (as) (a: aux)
            
lista2:: [String] -> String -> [Double]
lista2 [] _= []
lista2 (a:(mes:(b:(num:as)))) comp | comp == mes =  read num:lista2 as comp
                                   | otherwise = lista2 as comp


logMes :: String -> String -> Double
logMes mes fatura = foldl (+) 0 (lista2 (lista fatura "") mes)
--Q2

minMaxCartaoAux :: [String] -> (Double,Double) -> (Double, Double)
minMaxCartaoAux [] x = x
minMaxCartaoAux (a:(b:(c:(d:as)))) (numUm, numDois) | read d < numUm = (minMaxCartaoAux as) (read d, numDois)
                                                    | read d > numDois = (minMaxCartaoAux as) (numUm, read d)
                                                    | otherwise = minMaxCartaoAux as (numUm, numDois)

umMenor :: [String] -> Double
umMenor [] = 0
umMenor (a:(b:(c:(d:as)))) = read d

minMaxCartao :: String -> (Double,Double)
minMaxCartao fatura = minMaxCartaoAux (listaStr) (umMenor listaStr,0)
                        where listaStr = lista fatura ""
--Q3

isReplica :: String -> Int -> Char -> Bool
isReplica "" 0 _ = True
isReplica "" _ _ = False
isReplica (a:as) num charac | a /= charac = False
                            | otherwise = isReplica as (num-1) charac

--Q4

decEnigma :: String -> [(Char, Char)] -> String
decEnigma [] _ = []
decEnigma (a:as) lista = [b | (aa, b) <- lista, a==aa] ++ decEnigma as lista

--Q5

btoiAux :: String-> Int -> Int
btoiAux [] _ = 0
btoiAux (a:as) expoente = read ([a]) * (2 ^ expoente) + btoiAux as (expoente+1)

btoi :: String -> Int
btoi num = btoiAux (reverse num) 0

--Q6
type Comando = String
type Valor = Int
executaAux :: [(Comando, Valor)] -> Int -> Int
executaAux [] num = num
executaAux (("Divide", 0):as) _ = (-666)
executaAux ((comando,valor): as) num | comando == "Multiplica" = executaAux as (num *valor)
                                     | comando == "Soma" = executaAux as (num +valor)
                                     | comando == "Subtrai" =executaAux as (num - valor)
                                     | comando == "Divide" = executaAux as (div num valor)
                                     | otherwise = executaAux as valor

executa :: [(Comando, Valor)] -> Int
executa lista = executaAux lista 0

--Q7
mul2 :: [Int] -> [Int] -> [Int]
mul2 [] [] = []
mul2 [] (a:as) = 0: mul2 [] as
mul2 (a:as) [] = 0: mul2 as []
mul2 (a:as) (aa:aas) = a*aa: mul2 as aas

--Q1


data Maybe a = Just a | Nothing
               deriving(Show)

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond (a:[]) = Nothing
safeSecond (a:as) = Just (head as)

safeCalc :: String -> IO ()
safeCalc str | ope == "mul" =  putStrLn (show (Just (n1*n2)))
             | ope == "sub" = putStrLn (show (Just (n1-n2)))
             | ope == "sum" = putStrLn (show ((Just (n1+n2))))
             | otherwise = putStrLn (show (divSafe n1 n2))
            where (n1, ope, n2) = pegarStr str ("","") ""

pegarStr :: String -> (String, String) -> String-> (Int, String, Int)
pegarStr [] ("","") _ = (0,"",0)
pegarStr [] (n, ope) aux = (read n::Int, ope, read aux::Int)
pegarStr ('d':(_:(_:as))) _ aux = pegarStr as (aux, "div") ""
pegarStr (a:('u':(b:as))) _ aux = pegarStr as (aux, a:('u':[b]))  ""
pegarStr (a:as) (n, op) aux = pegarStr as (n,op) (aux++[a])

divSafe :: Int -> Int -> Maybe(Int)
divSafe _ 0 = Nothing
divSafe a b = Just (div a b)
