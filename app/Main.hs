-- MazeGame.hs
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- Define the data types for the game state
data GameState = GameState
  { 
    playerPos :: (Float, Float)
  , maze :: Picture
  }

initialState :: IO GameState
initialState = do
  randomMaze <- generateMazeWithPath
  return $ GameState
    { playerPos = (-350, 250)  -- Start at the top-left corner
    , maze = randomMaze
    }

-- Move the player based on the given offset
movePlayer :: (Float, Float) -> (Float, Float) -> (Float, Float)
movePlayer (dx, dy) (x, y) = (x + dx, y + dy)

-- Define the player
player :: Picture
player = translate 0 0 $ color playerColor $ circleSolid 20

-- Colors
wallColor, playerColor, pathColor :: Color
wallColor = makeColor 0.0 0.0 0.0 1.0  -- Black walls
playerColor = makeColor 0.0 0.0 1.0 1.0
pathColor = makeColor 1.0 1.0 1.0 1.0  -- White path

-- Render the game
renderGame :: GameState -> IO Picture
renderGame game = return $ pictures [maze game, translate (fst $ playerPos game) (snd $ playerPos game) player]

-- Event handling
handleEvent :: Event -> GameState -> IO GameState
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) game = tryMovePlayer (0, 10) game
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) game = tryMovePlayer (0, -10) game
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) game = tryMovePlayer (-10, 0) game
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) game = tryMovePlayer (10, 0) game
handleEvent _ game = return game

tryMovePlayer :: (Float, Float) -> GameState -> IO GameState
tryMovePlayer (dx, dy) game@(GameState { playerPos = (x, y), maze = mazePic }) =
  let newX = x + dx
      newY = y + dy
      collides = any (\pic -> isColliding newX newY pic) [mazePic]
  in return $ if collides then game else game { playerPos = (newX, newY) }

-- Check if a point collides with a picture
isColliding :: Float -> Float -> Picture -> Bool
isColliding x y (Translate tx ty pic) = isColliding (x - tx) (y - ty) pic
isColliding x y (Color _ (Polygon vertices)) = pointInPolygon (x, y) vertices
isColliding _ _ _ = False

-- Check if a point is inside a polygon
pointInPolygon :: (Float, Float) -> [Point] -> Bool
pointInPolygon (px, py) vertices =
  odd $ length $ filter (\((x1, y1), (x2, y2)) -> intersect (px, py) (1e8, py) (x1, y1) (x2, y2)) (zip vertices (tail (cycle vertices)))


-- Check if two line segments intersect
intersect :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
intersect (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
  let det = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
      ua = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / det
      ub = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / det
  in ua >= 0 && ua <= 1 && ub >= 0 && ub <= 1

-- Generate a maze with a path from the top-left to the bottom-right
generateMazeWithPath :: IO Picture
generateMazeWithPath = do
  let pathWidth = 40  -- Adjust the width of the path as needed
      pathHeight = 40  -- Adjust the height of the path as needed
      path = translate (-350) 250 $ color pathColor $ rectangleSolid (fromIntegral pathWidth) (fromIntegral pathHeight)
      fullMaze = pictures
        [ color wallColor $ rectangleSolid 800 600  -- Full screen black background
        , path
        ]
  return fullMaze

-- Update the game state
updateGame :: Float -> GameState -> IO GameState
updateGame _ = return

-- Main function
main :: IO ()
main = do
  initialState' <- initialState
  playIO (InWindow "Maze Game" (800, 600) (10, 10)) bgColor 60 initialState' renderGame handleEvent updateGame

bgColor :: Color
bgColor = makeColor 1.0 1.0 1.0 1.0  -- White background
