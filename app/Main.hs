import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.IO
import Data.List (elemIndices)

data GameState = GameState
    { maze :: [[Char]]
    , playerPos :: (Int, Int)
    , finished :: Bool
    }

initialState :: IO GameState
initialState = do
    contents <- readFile "level_1.txt"
    let mazeLines = lines contents
        start = findStart mazeLines
    return $ GameState mazeLines (0, 0) False

findStart :: [[Char]] -> (Int, Int)
findStart maze = case [(row, col) | (row, line) <- zip [0..] maze, col <- elemIndices 's' line] of
    [] -> error "No start position found."
    (pos:_) -> pos

wallColor, pathColor, startColor, finishColor, playerColor :: Color
wallColor = makeColorI 0 0 0 255 -- Black
pathColor = makeColorI 255 255 255 255 -- White
startColor = makeColorI 0 255 0 255 -- Green
finishColor = makeColorI 255 0 0 255 -- Red
playerColor = makeColorI 0 255 0 255 -- Green

screenWidth, screenHeight :: Int
screenWidth = 800
screenHeight = 600

render :: GameState -> Picture
render game = pictures [mazePic, playerPic]
  where
    scaleFactor = 30.0

    mazePic = pictures $ concatMap (\(i, row) ->
      map (\(j, cell) -> case cell of
          'x' -> color wallColor $ translate (fromIntegral j * scaleFactor) (fromIntegral i * scaleFactor) $ rectangleSolid scaleFactor scaleFactor
          ' ' -> color pathColor $ translate (fromIntegral j * scaleFactor) (fromIntegral i * scaleFactor) $ rectangleSolid scaleFactor scaleFactor
          's' -> color startColor $ translate (fromIntegral j * scaleFactor) (fromIntegral (length mazeLines - i - 1) * scaleFactor) $ rectangleSolid scaleFactor scaleFactor
          'e' -> color finishColor $ translate (fromIntegral j * scaleFactor) (fromIntegral (length mazeLines - i - 1) * scaleFactor) $ rectangleSolid scaleFactor scaleFactor
          _   -> blank
        ) $ zip [0..] row
      ) $ zip [0..] mazeLines

    playerPic = translate (fromIntegral x * scaleFactor) (fromIntegral y * scaleFactor) $ color playerColor $ rectangleSolid scaleFactor scaleFactor
      where
        (x, y) = playerPos game

    -- Add this line to fix the mirroring issue
    mazeLines = maze game


handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) game =
    movePlayer (0, 1) game  -- Invert the Y-axis movement
handleInput (EventKey (SpecialKey KeyDown) Down _ _) game =
    movePlayer (0, -1) game  -- Invert the Y-axis movement
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) game =
    movePlayer (-1, 0) game
handleInput (EventKey (SpecialKey KeyRight) Down _ _) game =
    movePlayer (1, 0) game
handleInput _ game = game

movePlayer :: (Int, Int) -> GameState -> GameState
movePlayer (dx, dy) game@(GameState maze (x, y) finished)
    | finished = game
    | isValidMove maze (x + dx, y + dy) = checkFinish $ GameState maze (x + dx, y + dy) False
    | otherwise = game

isValidMove :: [[Char]] -> (Int, Int) -> Bool
isValidMove maze (x, y) =
    x >= 0 && y >= 0 &&
    y < length maze &&
    x < length (maze !! y) &&
    (maze !! y !! x) /= 'x'  

checkFinish :: GameState -> GameState
checkFinish game@(GameState maze (x, y) _)
    | isFinish maze (x, y) = game { finished = True }
    | otherwise = game

isFinish :: [[Char]] -> (Int, Int) -> Bool
isFinish maze (x, y) = maze !! y !! x == 'e'

update :: Float -> GameState -> GameState
update _ = id

main :: IO ()
main = do
    initialState >>= \initial -> playIO (InWindow "Maze Game" (screenWidth, screenHeight) (10, 10)) white 60
        initial
        (\game -> return $ render game)
        (\event game -> return $ handleInput event game)
        (\_ game -> return $ update 0.1 game)
