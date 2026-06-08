import Data.Char (ord, chr, toLower, isLetter)

deslocarChar :: Int -> Char -> Char
deslocarChar n c
    | isLetter c = chr ((ord (toLower c) - ord 'a' + n) `mod` 26 + ord 'a')
    | otherwise  = c

cifraCesar :: Int -> String -> String
cifraCesar n s = map (deslocarChar n) s

main :: IO ()
main = do
    
    line1 <- getLine
    let n = read line1 :: Int

    msg <- getLine
    
    putStrLn (cifraCesar n msg)