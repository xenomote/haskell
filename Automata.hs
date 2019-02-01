module Automata where

    import Control.Monad.Writer
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

    -- Q1.

    data StateQ1 = S1 | S2 | S3 deriving (Eq, Show)
    data InputQ1 = A  | B       deriving (Eq, Show)

    tQ1 S1 B = S2
    tQ1 _  B = S3
    tQ1 _  A = S1

    aQ1 = (==) S2

    mQ1 = Automata S1 tQ1 aQ1

    _Q1A :: IO ()
    _Q1A = mapM_ (\l -> print (mQ1 `recognises` l, l))
        [
            [A, A, A, B, B],
            [A, B, B, B, A, B, B, B],
            [B, A, B, A, B, A],
            [A, A, A, B, A, B],
            [B, B, B, A, B, A, B]
        ]