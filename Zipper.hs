module Zipper where

    -- zlist must always have a focus
    data ZList a = ZList [a] a [a] deriving (Show, Eq)

    -- mapping over the zlist is mapping over all elements in the zlist
    instance Functor ZList where
        fmap f (ZList a b c) = ZList (map f a) (f b) (map f c)

    -- wrapping an element in a zlist sets it as the focus
    -- the focused function is applied to the zlist of elements in application
    instance Applicative ZList where
        pure a = ZList [] a []
        ZList _ f _ <*> a = fmap f a

    -- moves the focus the the next element if it isnt the last
    next :: ZList a -> Maybe (ZList a)
    next (ZList a b (b':c)) = Just (ZList (b:a) b' c)
    next (ZList _ _ []) = Nothing

    -- moves the focus to the previous element if it isnt the first
    prev :: ZList a -> Maybe (ZList a)
    prev (ZList (b':a) b c) = Just (ZList a b' (b:c))
    prev (ZList [] _ _ ) = Nothing

    -- returns the focus
    focus :: ZList a -> a
    focus (ZList _ a _) = a

    -- replaces the focus
    replace :: ZList a -> a -> ZList a
    replace l = insert l EQ

    -- LT inserts the element before the focus
    -- EQ replaces the current focus
    -- GT inserts the element after the focus
    insert :: ZList a -> Ordering -> a -> ZList a
    insert (ZList a b c) LT a' = ZList (a':a) b c
    insert (ZList a b c) EQ b' = ZList a b' c
    insert (ZList a b c) GT c' = ZList a b (c':c)