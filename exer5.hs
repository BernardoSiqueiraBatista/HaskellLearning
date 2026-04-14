type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa lista = conta lista 0

conta :: [(Comando, Valor)] -> Int -> Int
conta [] n = n
conta ((com, val):xs)  n | com == "Multiplica" =  conta xs (val * n)
                         | com == "Divide" && (val==0) = conta xs (-666)
                         | com == "Divide"  = conta xs (n `div` val)
                         | com == "Soma" = conta xs (n + val)
                         | com == "Subtrai" = conta xs (n-val)

main = do
    a <- getLine
    let result = executa (read a)
    print result
