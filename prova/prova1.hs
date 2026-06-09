data Time  = Haiti  | Escocia  | Brasil  | Marrocos  | Catar  | Suica  | Canada  | Bosnia 
    deriving (Show, Eq)

type Jogo = (Time, Int, Int, Time)
-- exemplo: (Brasil, 1, 0, Haiti) ----> significa Brasil 1 x 0 Haiti


jogos1 :: [Jogo]

jogos1 = [(Haiti,    1, 3, Escocia), (Brasil,  2, 0, Marrocos),
          (Catar,    0, 2, Suica),   (Canada,  0, 0, Bosnia),
          (Suica,    0, 1, Bosnia),  (Canada,  0, 0, Catar),
          (Brasil,   1, 0, Haiti),   (Escocia, 0, 1, Marrocos),
          (Suica,    1, 0, Canada),  (Bosnia,  1, 1, Catar),
          (Marrocos, 0, 0, Haiti),   (Escocia, 1, 2, Brasil)]



{-

1 - (2,0 pontos) Defina uma função que, dado um time e uma lista de jogos, informe quantos gols aquele time fez.
[esta questão tem mais 0,5 ponto se usar map/filter ou compreensões de listas na solução]
-}


{-gols :: Time -> [Jogo] -> Int
gols time [] = 0
gols time ((t1, g1, g2, t2):xs) | t1 == time = g1 + gols time xs
                                 | t2 == time = g2 + gols time xs
                                 | otherwise = gols time xs
-}

-- Testar usando map/filter
gols' :: Time -> [Jogo] -> Int
gols' time jogos = sum (map extraiGols jogosDoTime)
  where
 
    jogosDoTime = filter (\(t1, _, _, t2) -> t1 == time || t2 == time) jogos
    
    extraiGols (t1, g1, g2, _) | t1 == time = g1
                               | otherwise  = g2

--Testar usando list compheension
gols :: Time -> [Jogo] -> Int
gols time jogos = sum [if t1==time then g1 else g2 | (t1,g1,g2,t2)<-jogos, time==t1 || t1==time] 

    
{-2 - (2,0 pontos) Defina uma função que, dado um time e uma lista de jogos, informe quantos pontos ele obteve naquele conjunto de jogos. Lembrando que são 3 pontos por uma vitória, 1 ponto por um empate, e zero pontos por uma derrota.
[esta questão tem mais 0,5 ponto se usar map
/filter ou compreensões de listas na solução]
haskellpontos :: Time -> [Jogo] -> Int-}
haskellpontos :: Time -> [Jogo] -> Int
haskellpontos time [] = 0
haskellpontos time ((t1,g1,g2,t2) : xs) 
    |  t1 == time && g1 > g2 = 3 + haskellpontos time xs
    |  (t1 == time && g1 == g2) = 1 + haskellpontos time xs
    |  (t1 == time && g1 < g2) = 0 + haskellpontos time xs
    |  (t2 == time && g2 > g1) = 3 + haskellpontos time xs
    |  (t2 == time && g2 == g1) = 1 + haskellpontos time xs
    |  (t2 == time && g2 < g1) = 0 + haskellpontos time xs
    | otherwise = haskellpontos time xs

haskellpont :: Time -> [Jogo] -> Int
haskellpont time jogos = sum [if t1==time && g1>g2 then 3 else if (t1==time || t2==time) && g1==g2 then 1 else if t1==time && g1<g2
then 0 else if t2== time && g2>g1 then 3 else 0 | (t1,g1,g2,t2)<-jogos] 




{-3 - (2,0 pontos) Defina uma função que, dado um time e uma lista de jogos, qual o seu saldo de gols naquele conjunto de jogos (gols feitos - gols tomados).
[esta questão tem mais 0,5 ponto se usar map/filter ou compreensões de listas na solução]
haskellsaldo time jogos = sum [if t1==time then (g1-g2) else (g2-g1) | (t1,g1,g2,t2) <- jogos, t1==time || t2==time]
haskellsaldo :: Time -> [Jogo] -> Int-}
haskellsaldo :: Time -> [Jogo] -> Int
haskellsaldo time jogos = sum (map f (filter jogosTimes jogos))

    where jogosTimes (t1,_,_,t2)

                     | (t1==time || t2 == time) = True
                     |  otherwise = False

          f (t1,g1,g2,t2) | (t1==time) = (g1-g2)
                          | (t2==time) = (g2-g1) 
    
--4 - (2,5 pontos) Dado um grupo, representado por uma lista de 4 times, e um conjunto de jogos, 
--faça uma função que retorne a lista com os dois times classificados (os dois times com mais pontos). 
--Não é preciso tratar os casos em que mais de 2 times tem pontos para se classificar (mesmo número de pontos que o segundo).
--haskellclassificados :: [Time] -> [Jogo] -> [Time]
--classificados [Haiti, Escocia, Brasil, Marrocos] jogos1 ----> [Brasil, Marrocos]


-- função pra contar pontuaçoes de cada time
-- função para sortear cada time
-- função para pegar somente os dois primeiros

haskellclassificados :: [Time] -> [Jogo] -> [Time]
haskellclassificados (time:times) jogos = func (sortear (funch (time:times) jogos))
    where
        funch [] jogos = [] 
        funch (time:times) jogos = ((time, haskellpontos time jogos) : funch times jogos)

        sortear [] = []
        sortear(x:xs) = sortear [(timea,ponto)| (timea, ponto) <- xs , ponto > snd x] ++ [x] ++ sortear[(timea,ponto) | (timea, ponto) <- xs , ponto < snd x]

        func ((ta,_) : (tb,_) : _ ) = [ta,tb]







