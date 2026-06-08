data Viagem = Viagem String Float Int Bool deriving (Read, Show)

is_valid :: Viagem -> String -> Bool
is_valid (Viagem nome dist nota pico) app 

    | nota >= 4 && nome == app = True
    | otherwise                = False
r
custo_total :: Viagem -> Float -> Float
custo_total (Viagem nome dist nota pico) acumulador

    | nota == 5 && dist < 2 = 0 + acumulador
    | pico                  = custo + 5 + acumulador
    | otherwise             = custo + acumulador
    where custo = 2.5 * dist

custoTotal :: String -> [Viagem] -> Float 
custoTotal app viagens = foldr custo_total 0 (filter (\v -> is_valid v app) viagens)

main :: IO ()
main = do
    appAlvo <- getLine
    entradaLista <- getLine
    let viagens = read entradaLista :: [Viagem]
    print (custoTotal appAlvo viagens)
