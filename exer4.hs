addEspacos :: Int -> String
addEspacos n = replicate n ' '

paraDireita :: Int -> String -> String
paraDireita n s = addEspacos n ++ s

parseInput str = let [n, s] = words str
                 in (read n, s)
main :: IO()
main = interact $ uncurry paraDireita . parseInput