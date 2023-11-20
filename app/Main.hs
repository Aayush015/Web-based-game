import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data GameState = WelcomeScreen | Playing deriving (Eq)

data World = World
  { playerName :: String
  , gameState  :: GameState
  }

initialWorld :: World
initialWorld = World { playerName = "", gameState = WelcomeScreen }

windowWidth, windowHeight :: Int
windowWidth  = 800
windowHeight = 600

main :: IO ()
main = do
  play
    (InWindow "Maze Game" (windowWidth, windowHeight) (10, 10))
    white
    30
    initialWorld
    draw
    handleInput
    update

draw :: World -> Picture
draw world = case gameState world of
  WelcomeScreen -> pictures
    [ background
    , welcomeMessage
    , playerNamePrompt
    , playerNameText (playerName world)
    , startButton
    ]
  Playing -> Blank
  where
    background = color (light blue) $ rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight)
    welcomeMessage = translate (-150) 150 $ scale 0.5 0.5 $ text "WELCOME TO THE MAZE GAME"
    playerNamePrompt = translate (-100) 50 $ scale 0.25 0.25 $ text "Enter your name:"
    playerNameText name = translate (-30) 0 $ scale 0.25 0.25 $ text name
    startButton = translate (-50) (-100) $ color (dark green) $ rectangleSolid 100 40

handleInput :: Event -> World -> World
handleInput (EventKey (MouseButton LeftButton) Up _ (x, y)) world =
  case gameState world of
    WelcomeScreen ->
      if isWithinButtonBounds x y
        then world { gameState = Playing }
        else world
    Playing -> world
handleInput (EventKey (Char c) Down _ _) world =
  case gameState world of
    WelcomeScreen ->
      if c == '\DEL' && not (null (playerName world))
        then world { playerName = init (playerName world) }
        else world { playerName = playerName world ++ [c] }
    Playing -> world
handleInput _ world = world

update :: Float -> World -> World
update _ world = world

isWithinButtonBounds :: Float -> Float -> Bool
isWithinButtonBounds x y = x >= -50 && x <= 50 && y >= -120 && y <= -80
