type Time         = String
type Nome         = String
type Kills        = String
type Mortes       = String
type Assistencias = String

quickSort :: [(String, Double)] -> [(String, Double)]
quickSort [] = []
quickSort ((nome, nota):xs) =
    quickSort [(n,nt) | (n,nt) <- xs, nt > nota]
    ++ [(nome, nota)]
    ++ quickSort [(n,nt) | (n,nt) <- xs, nt < nota]

calculoKDA :: (Time, Nome, Kills, Mortes, Assistencias) -> Double
calculoKDA (t,n,k,m,a) = fromIntegral (round (resultado * 100)) / 100.0
    where resultado = (read k + read a) / fromIntegral (read m :: Int) :: Double

escolher :: String -> [(Time, Nome, Kills, Mortes, Assistencias)] -> [(String, Double)]
escolher decidir [] = []
escolher decidir ((t,n,k,m,a):xs)
    | t == decidir = (n, kda) : escolher decidir xs
    | otherwise    = escolher decidir xs
    where kda = calculoKDA (t,n,k,m,a)

pegarCadaCaracteristica :: String -> (String, String)
pegarCadaCaracteristica [] = ([], [])   
pegarCadaCaracteristica (a:as)
    | a == ';'  = ([], as)
    | otherwise = let (token, resto) = pegarCadaCaracteristica as
                  in  (a : token, resto)

pegarCadaJogador :: String -> [(Time, Nome, Kills, Mortes, Assistencias)]
pegarCadaJogador "" = []
pegarCadaJogador frase =
    (t, n, k, m, a) : pegarCadaJogador r5
    where
        (t, r1) = pegarCadaCaracteristica frase
        (n, r2) = pegarCadaCaracteristica r1
        (k, r3) = pegarCadaCaracteristica r2
        (m, r4) = pegarCadaCaracteristica r3
        (a, r5) = pegarCadaCaracteristica r4

pegarTodosOsJogadores :: String -> [(Time, Nome, Kills, Mortes, Assistencias)]
pegarTodosOsJogadores frase = pegarCadaJogador frase

rankTime :: String -> String -> [(String, Double)]
rankTime time lista = quickSort fase1
    where fase1 = escolher time (pegarTodosOsJogadores lista)

main :: IO ()
main = do
    time <- getLine
    s <- getLine
    let result = rankTime time s
    print result