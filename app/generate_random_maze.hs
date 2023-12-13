import System.Random
import Data.List
import System.IO

data Cell = Start | Empty | Wall | End deriving (Eq, Show)

type Maze = [[Cell]]

-- Function to generate a random maze
generateMaze :: Int -> Int -> IO Maze
generateMaze rows cols = do
  gen <- getStdGen
  let maze = take rows . chunksOf cols $ randomCells gen
  let updatedMaze = updateStartEnd maze
  return updatedMaze

-- Function to update the maze to ensure one 's' at the start and one 'e' at the end
updateStartEnd :: Maze -> Maze
updateStartEnd [] = []
updateStartEnd (row:rows) = updateStart row : updateEnd rows

updateStart :: [Cell] -> [Cell]
updateStart [] = []
updateStart (Start:rest) = Start : rest
updateStart (_:rest) = Empty : rest

updateEnd :: Maze -> Maze
updateEnd [] = []
updateEnd [row] = [updateEndRow row]
updateEnd (row:rows) = row : updateEnd rows

updateEndRow :: [Cell] -> [Cell]
updateEndRow [] = []
updateEndRow row = init row ++ [End]

-- Function to generate random cells (Empty, Wall)
randomCells :: RandomGen g => g -> [Cell]
randomCells gen = concatMap generateRow (randomRs (1, 2) gen)

-- Function to generate a row of random cells
generateRow :: Int -> [Cell]
generateRow n
  | n == 1 = [Empty]
  | n == 2 = [Wall]

-- Function to convert a row to a string
rowToString :: [Cell] -> String
rowToString = concatMap cellToChar

-- Function to convert a cell to a character
cellToChar :: Cell -> String
cellToChar Start = "s"
cellToChar Empty = " "
cellToChar Wall  = "x"
cellToChar End   = "e"

-- Function to write the maze to a file
writeMazeToFile :: Maze -> FilePath -> IO ()
writeMazeToFile maze filePath = do
  let mazeString = intercalate "\n" (map rowToString maze)
  writeFile filePath mazeString

-- Function to split a list into chunks of a given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

main :: IO ()
main = do
  let rows = 5
      cols = 10
      fileName = "level.txt"
  maze <- generateMaze rows cols
  writeMazeToFile maze fileName
  putStrLn $ "Maze generated and saved in " ++ fileName
