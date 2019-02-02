module Automata where

    import Data.Foldable (foldl')

    data Automata s a = Automata {
        initial    :: s,
        transition :: (s -> a -> s),
        accepts    :: (s -> Bool)
    }

    scan :: Foldable w => Automata s a -> w a -> s
    scan a w = foldl' (transition a) (initial a) w

    recognises :: Foldable w => Automata s a -> w a -> Bool
    recognises a w = a `accepts` scan a w

    shift :: Automata s a -> a -> Automata s a
    shift (Automata i t a) l = Automata (t i l) t a