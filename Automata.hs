module Automata where

    import Data.Foldable

    data Automata s a = Automata {
        initial    :: s,
        transition :: a -> s -> s,
        accepts    :: s -> Bool
    }

    scan :: Foldable f => Automata s a -> f a -> s
    scan m = foldl' (flip $ transition m) (initial m)

    recognises :: Foldable f => Automata s a -> f a -> Bool
    recognises m = accepts m . scan m