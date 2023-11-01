import System.Random

main :: IO ()
main = do
    putStrLn "Welcome to the Number Guessing Game!"
    playGame

playGame :: IO ()
playGame = do
    -- Generate a random number between 1 and 100
    randomNumber <- randomRIO (1, 100)
    putStrLn "I'm thinking of a number between 1 and 100. Can you guess it?"
    playGuessingGame randomNumber

playGuessingGame :: Int -> IO ()
playGuessingGame randomNumber = do
    putStr "Enter your guess: "
    input <- getLine
    let guess = read input :: Int
    if guess == randomNumber
        then do
            putStrLn "Congratulations! You guessed the correct number."
            playAgain
        else if guess < randomNumber
            then do
                putStrLn "Too low. Try again."
                playGuessingGame randomNumber
            else do
                putStrLn "Too high. Try again."
                playGuessingGame randomNumber

playAgain :: IO ()
playAgain = do
    putStr "Play again? (yes/no): "
    input <- getLine
    if input == "yes"
        then playGame
        else putStrLn "Thanks for playing. Goodbye!"