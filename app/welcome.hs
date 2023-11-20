import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data GameState = WelcomeScreen | Playing deriving (Eq)

data World = World
  { playerName :: String
  , gameState  :: GameState
  , input      :: String
  }

initialWorld :: World
initialWorld = World { playerName = "", gameState = WelcomeScreen, input = "" }

windowWidth, windowHeight :: Int
windowWidth  = 800
windowHeight = 600

main :: IO ()
main = playIO
  FullScreen
  blue
  30
  initialWorld
  draw
  handleInput
  update

draw :: World -> IO Picture
draw world = case gameState world of
  WelcomeScreen -> return $ pictures
    [ background
    , welcomeText
    , playerNamePrompt
    , playerNameText (playerName world)
    , startButton
    , inputBox (input world)
    ]
  Playing -> return Blank
  where
    background = color blue $ rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight)
    lineSpacing = 30 -- Adjust the spacing between lines
    welcomeText =
      pictures
        [ translate (-780) (fromIntegral windowHeight / 2 - 50) $ scale 1.5 1.5 $ text "Welcome to the"
        , translate (-550) (fromIntegral windowHeight / 2 - 50 - lineSpacing * 9) $ scale 1.5 1.5 $ text "MAZE game"
        ]
    playerNamePrompt = translate (-200) (-200) $ scale 0.5 0.5 $ text "Enter your name:"
    playerNameText name = translate (-50) (-250) $ scale 0.25 0.25 $ text name
    startButton =
      translate 0 (-350) $      -- position of the rectange
        color (dark green) $
          pictures
            [ rectangleSolid 200 60    -- size of the rectange
            , translate (-50) (-10) $ scale 0.25 0.25 $ color white $ text "Start"
            ]
    inputBox inp = translate (0) (-250) $ pictures
      [ rectangleWire 200 30
      , translate (-90) (-15) $ scale 0.2 0.2 $ text inp
      ]

handleInput :: Event -> World -> IO World
handleInput (EventKey (MouseButton LeftButton) Up _ (x, y)) world =
  case gameState world of
    WelcomeScreen ->
      if isWithinButtonBounds x y
        then return $ world { gameState = Playing }
        else return world
    Playing -> return world
handleInput (EventKey (Char c) Down _ _) world =
  case gameState world of
    WelcomeScreen ->
      if c == '\DEL' && not (null (input world))
        then return $ world { input = init (input world) }
        else return $ world { input = input world ++ [c] }
    Playing -> return world
handleInput _ world = return world

update :: Float -> World -> IO World
update _ world = return world

isWithinButtonBounds :: Float -> Float -> Bool
isWithinButtonBounds x y = x >= -100 && x <= 100 && y >= -200 && y <= -140
