module Cards where

    type Deck = [Card]
    data Card = Card {value :: Value, suit :: Suit} | Joker deriving (Eq)
    data Suit = Diamonds | Hearts | Clubs | Spades deriving (Show, Eq, Ord, Bounded, Enum)
    data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Eq, Ord, Bounded, Enum)

    instance Show Card where

        show Joker = "Joker"
        show (Card value suit) = show value ++ " of " ++ show suit

    showDeck :: Deck -> String
    showDeck = unlines . map (show) 

    deck = Joker : Joker : [Card value suit | suit <- [minBound..], value <- [minBound..]]

    interleave cards = interleave left right where
        
        twoStacks = splitAt (div (length cards) 2) cards
        left = shift $ fst twoStacks
        right = shift $ snd twoStacks

        shift xs = zipWith const (last xs : xs) xs

        interleave (a:as) (b:bs) = a : b : interleave as bs
        interleave [] bs = bs
        interleave as [] = as

    shuffle n cards = iterate interleave cards !! n