module Coin where

    import Data.List
    import Data.Maybe
    import Data.Ord
    import qualified Data.Map as Map

    -- each solution is either solved with the given coins or impossible
    data Solution = Solved (Map Int Int) Int | Failed deriving (Eq, Show)

    instance Semigroup Solution where
        -- combine existing solutions
        Solved a n <> Solution b m = Solution (Map.unionWith (+) a b) (n + m)
        Failed <> _ = Failed
        _ <> Failed = Failed

    instance Monoid Solution where
        mempty = Solved empty 0

    -- organises solutions by their cost, failures have maximum cost
    instance Ord Solution where
        Solved _ a <= Solved _ b = a <= b
        Failed <= Solved _ _ = False
        _ <= Failed = True

    getCoins (Solved coins _) = coins
    getNumCoins (Solved _ numCoins) = numCoins
    
    isSolved (Solved _ _) = True
    isSolved Failed = False

    -- generates optimal solutions for denominations amenable to greedy changemaking
    greedyChange coins = solutions where

        solutions = map solution [0..]

        solution 0 = Solved [] 0
        solution value = head [extend (solutions !! (value - coin)) coin | coin <- sortedCoins, coin <= value]

        sortedCoins = sortBy (flip compare) coins

    -- generates all optimal solutions for arbitrary denominations
    generalChange coins = solutions where

        -- creates a local cache of solutions
        solutions = map solution [0..]

        -- populates each position in the cache with the optimal solution
        solution 0 = Solved [] 0
        solution value = minimum [solve value coin | coin <- coins]

        -- calculates the solution when using the given coin
        solve value coin = if value < coin 
            then Failed 
            else (solutions !! (value - coin)) <> coin

    -- makes change for values in GBP denomimations
    gbp = greedyChange [5000, 2000, 1000, 500, 200, 100, 50, 20, 10, 5, 2, 1]

