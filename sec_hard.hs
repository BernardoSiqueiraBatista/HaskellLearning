type DiaMes = String
type Empresa = String
type Valor = String

pegarCadaCaracteristica :: String -> (String, String)
pegarCadaCaracteristica [] = ([], [])   
pegarCadaCaracteristica (a:as)
    | a == ';'  = ([], as)
    | otherwise = let (token, resto) = pegarCadaCaracteristica as
                  in  (a : token, resto)


dividirMes :: [String] -> String
dividirMes (x:xs)  | xs == [] = x
                   | otherwise = dividirMes xs

escolher :: String -> [(DiaMes,Empresa,Valor)] -> [String]
escolher decidir [] = []
escolher decidir ((da,ea,va):xs)
                | x == decidir = va : escolher decidir xs
                | otherwise    = escolher decidir xs
        where x = dividirMes (words da)
      

pegarCadaJogador :: String -> [(DiaMes,Empresa,Valor)]
pegarCadaJogador "" = []
pegarCadaJogador frase =
    (da,ea,va) : pegarCadaJogador r3
    where
        (da, r1) = pegarCadaCaracteristica frase
        (ea, r2) = pegarCadaCaracteristica r1
        (va, r3) = pegarCadaCaracteristica r2

pegarTodosOsJogadores :: String -> [(DiaMes,Empresa,Valor)]
pegarTodosOsJogadores frase = pegarCadaJogador frase


logMes :: String -> String -> Double
logMes mes lista = sum (map read (escolher mes (pegarCadaJogador lista)))


main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result
