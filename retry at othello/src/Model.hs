{-# LANGUAGE TemplateHaskell #-}

module Model (
    module Model,
    module Control.Lens,
    module Graphics.Gloss,
    module Graphics.Gloss.Interface.IO.Game) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    import Control.Lens
    import Control.Monad.Trans.Maybe
    import Data.Maybe

    newtype File = File String deriving Show

    type ScreenSize = (Int, Int)
    type BoardSize = (Int, Int)
    type Position = (Int, Int)
    type Piece = (Position, Color)

    data Player m = Player {getColor :: Color, getMove :: Board -> m (Maybe Piece)}
    type PlayerIO = Player IO

    data Board = Board BoardSize [Piece] Color
        deriving Show

    data GameState = StartMenu | Match (PlayerIO, PlayerIO) Board
        
    instance Show GameState where

        show StartMenu = "StartMenu"
        show (Match _ board) = "Match " ++ show board

    data Inputs = Inputs {
        
        _mousePosition :: (Float, Float),
        _pressedKeys :: [Key]
        
    } deriving Show

    data Game = Game {
        
        _gameState  :: GameState,
        _inputs     :: Inputs,
        _files      :: [File],
        _screenSize :: ScreenSize
    
    } deriving Show

    makeLenses ''Game
    makeLenses ''Inputs

    startGame = Game StartMenu initialInputs [] (0, 0)
    initialInputs = Inputs (0, 0) []

    emptyBoard size = Board size [] white
    newGame white black size = Match (white, black) (emptyBoard size)
        


