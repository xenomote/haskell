module Test where

    import Data.List

    tab = "    "
    quotes = ("'", "'")
    brackets = ("(", ")")
    squareBrackets = ("[", "]")
    curlyBrackets = ("{", "}")
    newLines = ("\n", "")
    
    data JSON 
        = Object [(String, JSON)] 
        | List [JSON] 
        | Text String 
        | Number Float 
        | Boolean Bool 
        | Null
        deriving Eq

    data JSONError 
        = NotObject JSON 
        | NotList JSON 
        | NotText JSON 
        | NotNumber JSON 
        | NotBoolean JSON 
        | BadIndex Int 
        | BadAttribute String 
        deriving (Show, Eq)

    instance Show JSON where

        show Null = "null"
        show (Boolean boolean) = show boolean
        show (Number number) = show number
        show (Text text) = bracket quotes $ text

        show (List items)
            = bracket squareBrackets
            . bracket newLines
            . indent
            . unlines
            . itemise
            . map show
            $ items
        
        show (Object attributes) 
            = bracket curlyBrackets
            . bracket newLines
            . indent
            . unlines
            . itemise
            . map showAttribute
            $ attributes

    showAttribute :: (String, JSON) -> String
    showAttribute (name, item) = (bracket quotes name) ++ " : " ++ show item

    indent :: String -> String
    indent = unlines . map (tab ++) . lines

    bracket :: (String, String) -> String -> String
    bracket (l,r) line = l ++ line ++ r

    itemise :: [String] -> [String]
    itemise [] = []
    itemise (item:[]) = [item]
    itemise (item:items) = (item ++ ",") : itemise items

    getAttribute :: String -> JSON -> Either JSONError JSON
    getAttribute name (Object attributes) = case lookFor name of

        Just (name, json) -> Right json
        Nothing -> Left $ BadAttribute name

        where lookFor name = find (\(n, o) -> n == name) attributes

    getAttribute _ json = Left $ NotObject json

    getItem :: Int -> JSON -> Either JSONError JSON
    getItem i (List items) = case index i of
        
        Just json -> Right json
        Nothing -> Left $ BadIndex i

        where 
            
            index i = if i < 0 then Nothing else index' i items

            index' 0 (item:items) = Just item
            index' _ [] = Nothing
            index' i (_:items) = index' (i-1) items

    getItem i json = Left $ BadIndex i

    getText :: JSON -> Either JSONError String
    getText (Text text) = Right text
    getText json = Left $ NotText json

    getNumber :: JSON -> Either JSONError Float
    getNumber (Number number) = Right number
    getNumber json = Left $ NotNumber json

    getBoolean :: JSON -> Either JSONError Bool
    getBoolean (Boolean boolean) = Right boolean
    getBoolean json = Left $ NotBoolean json

