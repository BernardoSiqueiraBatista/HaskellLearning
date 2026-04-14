
ordenaAlunos :: [(String, Int)] -> [(String, Int)]
ordenaAlunos [] = []
ordenaAlunos ((nome, nota):xs) = ordenaAlunos [(n,nt)| (n,nt)<-xs, n < nome] ++ [(nome, nota)] ++ ordenaAlunos [(n, nt) | (n, nt) <- xs, n > nome]

main::IO()
main = do
       let a = [("John",8),("Mary",10),("Arthur",7)]
       let c = ordenaAlunos a 
       print c
       