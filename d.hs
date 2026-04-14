calcular_media :: (String, [Double]) -> Double
calcular_media (nome, notas) = soma / 3
    where soma = foldl (+) 0 notas


processaAlunos :: [(String, [Double])] -> [(String, Double, String)]
processaAlunos [] = []
processaAlunos ((nome, nota) : xs)

    | media >= 7 = (nome, media, "Aprovado") : processaAlunos xs
    | media >= 5 && media < 7 = (nome, media, "Recuperacao") : processaAlunos xs
    | otherwise = (nome, media, "Reprovado") : processaAlunos xs
    where media = calcular_media (nome, nota)
                  
main = do
    s <- getLine
    let entrada = read s :: [(String, [Double])]
    let result = processaAlunos entrada
    print result