module Automata where

    import Control.Monad
    import Data.List (nub)

    import Automata.Deterministic
    import qualified Automata.Nondeterministic as NFA

    convert :: (Monad t, Foldable t) => NFA.Automata t s a -> Automata (t s) a
    convert m = Automata initial transition accepts where

        initial = return $ NFA.initial m

        accepts = any $ NFA.accepts m

        transition a s = join $ fmap (m `NFA.transition` a) s