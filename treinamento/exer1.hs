isSorted :: Ord t => [t] -> Bool
isSorted [] = True
isSorted[_] = True
isSorted (x:y:zs) = x<=y && isSorted (y:zs)

bSort :: Ord t => [t] -> [t]
bSort [] = []
bSort [x] = [x] 
bSort(x:y:ys) | x<=y = x:bSort(y: ys)
              | otherwise = y:bSort( x: ys)

buble :: Ord t => [t] -> [t]
buble xs
    | isSorted xs = xs
    | otherwise = buble (bSort xs)