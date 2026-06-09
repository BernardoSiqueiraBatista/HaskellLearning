
qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = qSort [y| y <-xs, y<=x ] ++ [x] ++ qSort [y| y<-xs, y >=x ]

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio", "LivrsoDoSergio"),("Andre", "LivroDoAndre"), ("Sergio", "SegundoLivroDoSergio")]

livros:: BancoDados -> Pessoa -> [Livro]
livros banco p  = [l | (n, l) <- banco , p == n]

emprestimos:: BancoDados -> Livro -> [Pessoa]
emprestimos banco livro  = [n | (n, l) <- banco ]

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f x | f (x+1) >= f x = True
               | otherwise = False

somaQuadradoDosItens :: [Int] -> Int
somaQuadradoDosItens lista = foldr f 0 lista
      where f elemento acumulador = (elemento^2) + acumulador

filtrarElementos :: [Int] -> [Int]
filtrarElementos lista = filter (\x -> (x>0)) lista

filtrar  l = [x| x<-l , (x>0)]

mapearFunc :: [Int] -> [Int]
mapearFunc l = map f l
          where f x = x^2

data Pessoa t = Pessoa t

isPessoa :: Show t => Pessoa t -> Bool
isPessoa (Pessoa _) = True