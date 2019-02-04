module AnswerSheet (Section (..)) where

    data Section = Section {
        part        :: String,
        question    :: [String],
        answer      :: [String],
        subsections :: [Section]
    }

    instance Show Section where
        show = unlines . lines where

            lines :: Section -> [String]
            lines (Section part question answer subsections) = part : question
                ++ [""]
                ++ answer
                ++ [""]
                ++ (indented . joined . prefixed $ subsections) where

                prefixed :: [Section] -> [Section]
                prefixed = fmap (prefix part)

                joined :: [Section] -> [String]
                joined = concat . fmap lines

    indented :: [String] -> [String]
    indented = fmap ((++) "  ")

    prefix :: String -> Section -> Section
    prefix p s = s {part = p ++ "." ++ part s}