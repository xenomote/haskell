module Automata.Nondeterministic where

    import Automata

    nfsa :: (Monad t, Foldable t)
        => s                -- initial state
        -> (a -> s -> t s)  -- transfer function
        -> (s -> t s)       -- epsilon move function
        -> (s -> Bool)      -- accept state function
        -> Automata (t s) a
    nfsa i t e a = Automata initial transition accept where
        
        initial = return i

        -- a -> t s -> t s 
        transition a s = s >>= t a >>= e where

        -- t s -> Bool
        accept = any a