data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

maxTree :: Ord t => Tree t -> t
maxTree (Node b Nilt Nilt) = b
maxTree (Node b e Nilt)    = max b (maxTree e)
maxTree (Node b Nilt d)    = max b (maxTree d)
maxTree (Node b e d)       = max b (max eMax dMax)
  where eMax = maxTree e
        dMax = maxTree d

minTree :: Ord t => Tree t -> t
minTree (Node b Nilt Nilt) = b
minTree (Node b e Nilt)    = min b (minTree e)
minTree (Node b Nilt d)    = min b (minTree d)
minTree (Node b e d)       = min b (min eMin dMin)
  where eMin = minTree e
        dMin = minTree d

dfs :: Ord t => Tree t -> Bool
dfs Nilt                = True
dfs (Node b Nilt Nilt)  = True
dfs (Node b e Nilt)     = (b > maxTree e) && dfs e
dfs (Node b Nilt d)     = (b < minTree d) && dfs d
dfs (Node b e d)        = (b > maxTree e) && (b < minTree d) && dfs e && dfs d

isBST :: Ord t => Tree t -> Bool
isBST arvore
  | dfs arvore = True
  | otherwise  = False

main :: IO ()
main = do
  s <- getLine
  let result = isBST (read s :: Tree Int)
  print result