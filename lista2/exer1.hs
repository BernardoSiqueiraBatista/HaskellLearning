data Tipo = Fogo | Agua | Grama deriving (Read, Show, Eq)

data Pokemon = Pokemon String Tipo Int deriving (Read, Show)

batalha :: Pokemon -> Pokemon -> String
batalha (Pokemon nome1 tipo1 poder1)
        (Pokemon nome2 tipo2 poder2)

    | poderFinal1 >= poderFinal2 = nome1 ++ " venceu!"
    | otherwise                  = nome2 ++ " venceu!"

    where
        poderFinal1 = calculaPoder tipo1 tipo2 poder1
        poderFinal2 = calculaPoder tipo2 tipo1 poder2

calculaPoder :: Tipo -> Tipo -> Int -> Int
calculaPoder atacante defensor poder

    | vantagem atacante defensor = poder * 2
    | vantagem defensor atacante = poder `div` 2
    | otherwise                  = poder

vantagem :: Tipo -> Tipo -> Bool
vantagem Fogo Grama = True
vantagem Grama Agua = True
vantagem Agua Fogo  = True
vantagem _ _        = False

main = do
    entrada1 <- getLine
    entrada2 <- getLine

    let p1 = read entrada1 :: Pokemon
    let p2 = read entrada2 :: Pokemon

    print (batalha p1 p2)