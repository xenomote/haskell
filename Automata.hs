module Automata where

    import Automata.Deterministic
    import qualified Automata.Nondeterministic as NFA

    convert :: Traversable t => NFA.Automata t s a -> Automata (t s) a
    convert m = Automata initial transition accepts where

        initial :: t s  
        initial = undefined

        accepts = any (NFA.accepts m)

        transition :: a -> t s -> t s
        transition a s = undefined

    elements :: (Enum a, Bounded a) => [a]
    elements = [minBound ..]