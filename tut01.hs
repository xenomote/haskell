module Main where

    import AnswerSheet
    import Automata

    main :: IO ()
    main = print $

        Section "1"
        ["Consider the following DFA:"]
        (...)
        [
            Section "a"
            ["For each of the following words determine whether or not it is accepted by the",
             "DFA: aaabb, abbbabbb, bababa, aaabab, bbbabab."]
            (
                fmap (\w -> show w ++ " => " ++ show (mQ1 `recognises` w)) [
                    [A, A, A, B, B],
                    [A, B, B, B, A, B, B, B],
                    [B, A, B, A, B, A],
                    [A, A, A, B, A, B],
                    [B, B, B, A, B, A, B]
                ]
            )
            (...),

            Section "b"
            ["Find four more words that are accepted by the DFA, and four that are not."]
            (map show $ (take 4 $ findWords mQ1 True) ++ (take 4 $ findWords mQ1 False))
            (...),

            Section "c"
            ["Prove that there are infinitely many words accepted by the DFA, as well as in-",
            "finitely many words that are not accepted by it."]
            ["No other state is maintained than the state of the machine, so to prove that",
            "there are infinitely many accepted or rejected sequences then we need to prove",
            "that there is a loop containing such a state.",
            "Rejected: S1 -> S1, S1 -> S2 -> S1, S1 -> S2 -> S3 -> S1",
            "Accepted: S1 -> S2 -> S2, S1 -> S2 -> S1 -> S2, S1 -> S2 -> S3 -> S1 -> S2"]
            (...),

            Section "d"
            ["Find a regular expression for the language accepted by the DFA."]
            (...)
            (...)

        ]

        where (...) = []

    -- Q1.

    data StateQ1 = S1 | S2 | S3 deriving (Eq, Show, Read, Enum, Bounded)
    data InputQ1 = A  | B       deriving (Eq, Show, Read, Enum, Bounded)

    tQ1 B S1 = S2
    tQ1 B _  = S3
    tQ1 A _  = S1

    aQ1 = (==) S2

    mQ1 = Automata S1 tQ1 aQ1

    findWords :: (Enum a, Bounded a) => Automata s a -> Bool -> [[a]]
    findWords m p = filter (\w -> m `recognises` w == p) sequences

    sequences :: (Enum a, Bounded a) => [[a]]
    sequences = wrap elements ++ [a:w | w <- sequences,  a <- elements]

    elements :: (Enum a, Bounded a) => [a]
    elements = [minBound ..]

    wrap :: [a] -> [[a]]
    wrap = map (:[])