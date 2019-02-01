module Controller where

    import Data.List
    import Model

    handleInputsIO :: Event -> Game -> IO Game
    handleInputsIO (EventMotion position)                 game = return $ set (inputs . mousePosition) position game
    handleInputsIO (EventResize size)                     game = return $ set screenSize size game
    handleInputsIO (EventKey key keyState modifiers _)    game = let
        
        updateKeys :: [Key] -> [Key]
        updateKeys  = (if keyState == Down then insert else delete) key
        insert      = (:)

        in return $ over (inputs . pressedKeys) updateKeys game

    anyPressed :: Game -> Bool
    anyPressed = not . null . view (inputs . pressedKeys)

    pressed :: Key -> Game -> Bool
    pressed key = elem key . view (inputs . pressedKeys) 

    