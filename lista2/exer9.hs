data Nacao = Agua | Terra | Fogo | Ar deriving (Read, Show, Eq)

data Ataque = Simples Nacao Int
            | Combinado Nacao Nacao Int
            | EstadoAvatar Int
            deriving (Read, Show)
            

vantagem :: Ataque -> Ataque -> Bool
vantagem (Combinado Agua  Fogo  _) (Combinado Fogo  Agua  _) = True
vantagem (Combinado Agua  Terra _) (Combinado Terra Agua  _) = True
vantagem (Combinado Terra Ar    _) (Combinado Ar    Terra _) = True
vantagem (Combinado Ar    Fogo  _) (Combinado Fogo  Ar    _) = True

vantagem (Combinado Agua  Fogo  _) (Simples Fogo  _) = True
vantagem (Combinado Agua  Terra _) (Simples Terra _) = True
vantagem (Combinado Terra Ar    _) (Simples Ar    _) = True
vantagem (Combinado Ar    Fogo  _) (Simples Fogo  _) = True

vantagem (Simples Agua  _) (Simples Fogo  _) = True
vantagem (Simples Terra _) (Simples Agua  _) = True
vantagem (Simples Ar    _) (Simples Terra _) = True
vantagem (Simples Fogo  _) (Simples Ar    _) = True

vantagem (Simples Agua  _) (Combinado Fogo  Fogo  _) = True
vantagem (Simples Terra _) (Combinado Agua  Agua  _) = True
vantagem (Simples Ar    _) (Combinado Terra Terra _) = True
vantagem (Simples Fogo  _) (Combinado Ar    Ar    _) = True
vantagem _ _ = False

tem_estadoAvatar :: Ataque -> bool -> bool
tem_estadoAvatar EstadoAvatar _ t | (t==True) = False
                                  | otherwise = True


danoTotal :: Nacao -> [Ataque] -> Int
danoTotal n ataques = aux ataques (Simples n 0)
    where aux [] _ = 0
          aux (ataque:resto) anterior
            | vantagem anterior ataque = aux resto ataque + 10
            | otherwise                = aux resto ataque + 5

main = do
    a <- getLine
    b <- getLine
    let result = danoTotal (read a) (read b)
    print result