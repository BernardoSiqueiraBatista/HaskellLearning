{-Exercícios

• Defina uma função que, dado um valor inteiro s e
um número de semanas n, retorna quantas
semanas de 0 a n tiveram vendas iguais a s. Para
resolver esta questão, primeiro construa uma
definição simples para vendas.

• Defina uma função que, dado um número inteiro,
determina se ele é primo ou não.

Exercícios

• Defina uma função que, dados dois números
inteiros x e y, determina se esses números
são primos entre si*:

> primosEntreSi 20 21
True
> primosEntreSi 400 30
False

* http://www.matematicadidatica.com.br/NumerosPrimosEntreSi.aspx

Exercícios

• Defina as seguintes funções:
– fatorial
fat :: Int -> Int
– compara se quatro números são iguais
all4Equal :: Int -> Int -> Int -> Int -> Bool
– Defina all4Equal usando allEqual
– retorna quantos parâmetros são iguais
equalCount :: Int -> Int -> Int -> Int
-}

contaElementos:: Int -> Int -> Int
contaElementos s n | n<0 = 0
                   | vendas n == s = contaElementos s (n-1) + 1
                   | otherwise = contaElementos s (n-1)
    where vendas 0 = 0
          vendas x = x+3


{-definirPrimo :: Int -> Bool
definirPrimo n
  | n <= 1 = False  -- números <= 1 não são primos
  | n == 2 = True   -- 2 é primo
  | otherwise = dividir n 2
  where
    dividir x y
      | fromIntegral y > sqrt (fromIntegral x) = True
      | x `mod` y == 0 = False  -- se divide exato, não é primo
      | otherwise = dividir x (y + 1)
-}
definirPrimo :: Int -> Bool
definirPrimo n
  | n <= 1 = False
  | n == 2 = True
  | otherwise = dividir n 2
  where
    dividir x y
      | y * y > x = True  
      | x `mod` y == 0 = False
      | otherwise = dividir x (y + 1)



--Abordaagem Errada
{-primosEntreSi::Int->Int->Bool
primosEntreSi x y | dividir x 2 && dividir y 2  = False
                  | otherwise = True
                  where dividir a b 
                               | a==b = False  
                               | a `mod` b == 0 = True
                               | otherwise = dividir a (b + 1)
-}    
--Em haskell /= 
--Para construir essa ideia de verificar divisor comum pode-se utilizar
primosEntreSiD::Int->Int->Bool
primosEntreSiD x y = not (temDivisorComum x y 2)
               where temDivisorComum a b d 
                           |d > min a b = False
                           | a `mod` d ==0 && b `mod` d ==0 = True
                           | otherwise = temDivisorComum x y (d+1)
                           --Algorimto de Euclides
primosEntreSi::Int->Int->Bool
primosEntreSi x y = mdc x y == 1
              where 
                mdc a 0 = a
                mdc a b = mdc b (a `mod`b)

fat :: Int-> Int
fat 0 = 1
fat 1 = 1
fat x = x * fat(x-1)

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal x y z w = equalCount x y z w == 4
  where
    equalCount a b c d = length ( filter (\x->x==a) [b, c, d]) + 1

addEspacos:: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos (n-1) 

paraDireita :: Int->String->String
paraDireita n frase = addEspacos n ++ frase

menorMaior:: Int-> Int-> Int -> (Int, Int)
menorMaior a b c = (min a (min b c), max a (max b c))

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c) = (menor, meio, maior)
  where
    menor = minimum [a, b, c]
    maior = maximum [a, b, c]
    meio = a + b + c - menor - maior  -- soma total menos extremos