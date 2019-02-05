module Automata.Nondeterministic where

    import Data.Foldable

    data Automata f s a = Automata {
        initial    :: s,
        transition :: a -> s -> f s,
        accepts    :: s -> Bool
    }

    scan :: (Monad f, Foldable f, Foldable g) => Automata f s a -> g a -> f s
    scan m w = foldl' (flip $ transition m) (initial m) w