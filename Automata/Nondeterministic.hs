module Automata.Nondeterministic where

    data Automata f s a = Automata {
        initial    :: s,
        transition :: s -> a -> f s,
        accepts    :: s -> Bool
    }