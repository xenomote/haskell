module Automata.Nondeterministic where

    import Data.Foldable
    import Control.Monad
    import Data.List(nub)

    data Automata f s a = Automata {
        initial    :: s,
        transition :: a -> s -> f s,
        accepts    :: s -> Bool
    }

    scan :: (Monad f, Foldable f) => Automata f s a -> f a -> f s
    scan m = foldlM (flip $ transition m) (initial m)

    recognises :: (Monad f, Foldable f) => Automata f s a -> f a -> Bool
    recognises m = any (accepts m) . scan m

