module View where

    import Model

    renderGameIO :: Game -> IO Picture
    renderGameIO game = return $ case view gameState game of
        
        StartMenu -> renderMenu game
        Match _ _ _ -> renderMatch game

    renderMenu :: Game -> Picture
    renderMenu game = centered menu where

        centered = Translate (-x) 0

        menu = Color black
             $ Scale 0.25 0.25
             $ Pictures [title, start]
        
        title = Translate 0 y
              $ Text "Welcome to Othello!"

        start = Translate 0 (-y) 
              $ Text "Click anywhere to start"

        half x = x / 2
        (x, y) = view screenSize game & over each (half . fromIntegral)

    renderMatch :: Game -> Picture
    renderMatch game = Pictures
        [ renderBoard $ view (board gameState) game ]

    renderBoard :: Board -> Picture
    renderBoard board = Pictures
        [ rectangleSolid 