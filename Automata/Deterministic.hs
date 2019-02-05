module Automata.Deterministic where

    import Data.Foldable

    data Automata s a = Automata {
        initial    :: s,
        transition :: a -> s -> s,
        accepts    :: s -> Bool
    }

    scan :: Foldable f => Automata s a -> f a -> s
    scan m w = foldl' (flip $ transition m) (initial m) w

    recognises :: Foldable f => Automata s a -> f a -> Bool
    recognises a w = a `accepts` scan a w