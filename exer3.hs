
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras p = [(a,b,c) | a <- [1..p],b <- [a+1..p],c <- [b+1..p], a^2 + b^2 == c^2]

main = do
    s <- getLine
    let result = pitagoras (read s)
    print result