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
            (
                map show $ take 4 (findWords mQ1 False) ++ take 4 (findWords mQ1 True)
            )
            (...)
        ]

        where (...) = []

    -- Q1.

    data StateQ1 = S1 | S2 | S3 deriving (Eq, Show)
    data InputQ1 = A  | B       deriving (Eq, Show, Enum)

    tQ1 S1 B = S2
    tQ1 _  B = S3
    tQ1 _  A = S1

    aQ1 = (==) S2

    mQ1 = Automata S1 tQ1 aQ1


    findWords :: Enum a => Automata s a -> Bool -> [[a]]
    findWords m p = if (m `accepts` initial m)
        then []
        else concat $ fmap (\a -> fmap (\w -> a:w) $ findWords (m `shift` a) p) letters 
            
            where
            
            letters :: Enum a => [a]
            letters = (enumFrom $ toEnum 0)




        
