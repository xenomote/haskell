module Logic where

    import Model
    import Controller

    updateGameIO :: Float -> Game -> IO Game
    updateGameIO time game = return $ case view gameState game of 
        
        StartMenu -> menuLogic game
        Match _ _ _ -> undefined

        
        
    menuLogic game = if anyPressed game 
        then set gameState (newGame undefined undefined undefined) game 
        else game

