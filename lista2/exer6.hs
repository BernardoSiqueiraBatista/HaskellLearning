data Tipo =
Fogo
| Agua
| Planta
| Eletrico
| Psiquico
deriving (Eq, Read, Show)

data Pokemon =
Pokemon String Tipo Int Int
deriving (Read, Show)

--Pokemon "Nome" Tipo HP Ataque

calcular_pont:: Pokemon -> Int
calcular_pont (Pokemon n t h a) | (t == Fogo)  = a + h +10                                
|(t == agua) = a + h + 8
|(t == Planta )= a + h + 7
|(t == Eletrico) = a + h + 9
|(t == Psiquico) = a + h + 12

calcular_tipo:: Pokemon -> String
calcular_tipo p 
|(calcular_pont p>=180) = "Lendario"
| calcular_pont p < 120 = Normal
| otherwise = Forte
 
calcular_frase:: Pokemon -> (String, Int, String)
calcular_frase (Pokemon n t h a) = 
  (n, calcular_pont (Pokemon n t h a), calcular_tipo (Pokemon n t h a))

processaPokemons :: [Pokemon] -> [(String, Int, String)]
processaPokemons  entrada1 =  map calcular_frase entrada1 


main = do
s <- getLine
let entrada = read s :: [Pokemon]
print (processaPokemons entrada)