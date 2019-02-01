module Main where

    import Control.Monad

    import Model
    import View
    import Controller
    import Logic
    import Player

    window = InWindow "Othello" (500, 500) (0, 0)

    main :: IO()
    main = do
        
        
        playIO window white 120 startGame renderGameIO handleInputsIO updateGameIO
