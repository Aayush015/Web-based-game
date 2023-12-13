import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.IO
import Data.List (elemIndices)
import System.Exit
import Control.Monad.IO.Class (liftIO)


data GameState = GameState
    { maze :: [[Char]]
    , playerPos :: (Int, Int)
    , finished :: Bool
    , elapsedTime :: Float
    }

initialState :: IO GameState
initialState = do
    contents <- readFile "level_1.txt"
    let mazeLines = lines contents
        start = findStart mazeLines
    return $ GameState mazeLines (0, 0) False 0

findStart :: [[Char]] -> (Int, Int)
findStart maze = case [(row, col) | (row, line) <- zip [0..] maze, col <- elemIndices 's' line] of
    [] -> error "No start position found."
    (pos:_) -> pos

wallColor, pathColor, startColor, finishColor, playerColor, scoreColor :: Color
wallColor = makeColorI 0 0 0 255 -- Black
pathColor = makeColorI 255 255 255 255 -- White
startColor = makeColorI 0 255 0 255 -- Green
finishColor = makeColorI 255 0 0 255 -- Red
playerColor = makeColorI 0 255 0 255 -- Green
scoreColor = makeColorI 255 255 255 255 -- White

screenWidth, screenHeight, mazeWidth, scoreWidth :: Int
screenWidth = 1000 -- Adjust the total width as needed
screenHeight = 600
mazeWidth = screenWidth `div` 2
scoreWidth = screenWidth - mazeWidth

render :: GameState -> Picture
render game = pictures [mazePic, playerPic, scorePic, timerPic, highestScorePic]
  where
    scaleFactor = 29.0

    -- Adjust the translation values for the entire maze
    mazePic = translate (-fromIntegral screenWidth / 2 + fromIntegral mazeWidth / 2) (fromIntegral screenHeight / 4) $ pictures $ concatMap (\(i, row) ->
      map (\(j, cell) -> case cell of
          'x' -> color wallColor $ translate (fromIntegral j * scaleFactor) (-fromIntegral i * scaleFactor) $ rectangleSolid scaleFactor scaleFactor
          ' ' -> color pathColor $ translate (fromIntegral j * scaleFactor) (-fromIntegral i * scaleFactor) $ rectangleSolid scaleFactor scaleFactor
          's' -> color startColor $ translate (fromIntegral j * scaleFactor) (-fromIntegral i * scaleFactor) $ rectangleSolid scaleFactor scaleFactor
          'e' -> color finishColor $ translate (fromIntegral j * scaleFactor) (-fromIntegral i * scaleFactor) $ rectangleSolid scaleFactor scaleFactor
          _   -> blank
        ) $ zip [0..] row
      ) $ zip [0..] mazeLines

    -- Adjust the translation values for the player
    playerPic = translate (fromIntegral x * scaleFactor - fromIntegral screenWidth / 2 + fromIntegral mazeWidth / 2) (-(fromIntegral y * scaleFactor) + fromIntegral screenHeight / 4) $ color playerColor $ rectangleSolid scaleFactor scaleFactor
      where
        (x, y) = playerPos game

    scorePic = translate (fromIntegral (screenWidth `div` 4)) (fromIntegral (screenHeight `div` 2)) $ color (makeColorI 0 0 0 255) $ scale 0.5 0.5 $ text "Scoreboard"

    timerPic = translate (fromIntegral (screenWidth `div` 4)) (fromIntegral (screenHeight `div` 2) - 50) $ color (makeColorI 0 0 0 255) $ scale 0.5 0.5 $ text $ "Time: " ++ show (round $ elapsedTime game)

    highestScorePic = translate (fromIntegral (screenWidth `div` 4)) (fromIntegral (screenHeight `div` 2) + 50) $ color (makeColorI 0 0 0 255) $ scale 0.5 0.5 $ text $ "Highest: 95" 

    mazeLines = maze game


handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) game =
    movePlayer (0, -1) game  
handleInput (EventKey (SpecialKey KeyDown) Down _ _) game =
    movePlayer (0, 1) game  
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) game =
    movePlayer (-1, 0) game
handleInput (EventKey (SpecialKey KeyRight) Down _ _) game =
    movePlayer (1, 0) game
handleInput _ game@(GameState _ _ True _) = game -- Game already finished
handleInput _ game@(GameState maze (x, y) False elapsedTime)
    | isFinish maze (x, y) = GameState maze (x, y) True elapsedTime
    | otherwise = game


movePlayer :: (Int, Int) -> GameState -> GameState
movePlayer (dx, dy) game@(GameState maze (x, y) finished elapsedTime)
    | finished = game
    | isValidMove maze (x + dx, y + dy) = checkFinish $ GameState maze (x + dx, y + dy) False elapsedTime
    | otherwise = game

isValidMove :: [[Char]] -> (Int, Int) -> Bool
isValidMove maze (x, y) =
    x >= 0 && y >= 0 &&
    y < length maze &&
    x < length (maze !! y) &&
    (maze !! y !! x) /= 'x'  

checkFinish :: GameState -> GameState
checkFinish game@(GameState maze (x, y) _ elapsedTime)
    | isFinish maze (x, y) = game { finished = True }
    | otherwise = game

isFinish :: [[Char]] -> (Int, Int) -> Bool
isFinish maze (x, y) = maze !! y !! x == 'e'

update :: Float -> GameState -> GameState
update dt game
    | finished game = game
    | otherwise = game { elapsedTime = elapsedTime game + scalingFactor * dt, finished = isFinish (maze game) (playerPos game) }
    where
        scalingFactor = 1.0  

main :: IO ()
main = do
    initialState >>= \initial -> playIO FullScreen white 60
        initial
        (\game -> return $ render game)
        (\event game -> return $ handleInput event game)
        (\_ game -> return $ update 0.1 game)