module Automata.Nondeterministic where

    import Control.Monad
    import Data.Foldable
    import Automata

    nfsa :: (Alternative t, Monad t)
        => s                -- i
        -> (s -> t s)       -- e
        -> (a -> s -> t s)  -- t
        -> (s -> Bool)      -- a
        -> Automata (t s) a
    nfsa i e t a = Automata initial transition accept where
        
        initial = return i

        -- a -> t s -> t s 
        transition a s = let s' = t a s in s' <|> join $ fmap e s'

        -- t s -> Bool
        accept = any a
