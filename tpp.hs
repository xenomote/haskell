module Tpp where

    import Data.List

    --helpful where you can remove a digit and make a factor

    toDigits :: Int -> [Int]
    toDigits i = if i < 10 then [i] else mod i 10 : toDigits (div i 10)

    fromDigits :: [Int] -> Int
    fromDigits [] = 0
    fromDigits l = f' l 0 where
        f' [] _ = 0
        f' (a:l) x = a * (10^x) + f' l (x+1)

    dropOne :: [a] -> [[a]]
    dropOne l = zipWith (++) (inits l) (tail $ tails l) 

    factors :: Int -> [Int]
    factors i = fmap fromDigits $ dropOne (toDigits i)

    helpfuls :: Int -> [Int]
    helpfuls i = filter (\x -> if x == 0 then False else mod i x == 0) $ factors i

    fabulouses :: Int -> [Int]
    fabulouses i = helpfuls i >>= helpfuls

    findFabulouses :: [Int]
    findFabulouses = filter (\i ->not $ null (fabulouses i)) [0..9876543210]   
    
    maxAndLen :: [Int] -> (Int, Int)
    maxAndLen (i:[]) = (i, 1)
    maxAndLen (i:s)  = let (m, l) = maxAndLen s in (m, l+1)
    maxAndLen [] = (0, 0)

    answer = maxAndLen findFabulouses

