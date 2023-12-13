![Trial](https://docs.google.com/document/d/1_dzKuc3kNDMKZHtBYryaOYbox32Iz7ScJbc5UcypWOw/edit?usp=sharing)
# Maze Game

## Description
This is a simple maze game implemented in Haskell using the Gloss library.

## Features
- Navigate through a maze to reach the exit which is at the bottom right.
- Timer to track your progress.
- Restart the game at any time.

## How to Play
- Use the arrow keys (Up, Down, Left, Right) to navigate the player through the maze.
- The goal is to reach the exit marked with 'e' (marked red in the GUI).
- Avoid walls ('x')(marked black in the GUI) and reach the exit as quickly as possible.

## Controls
- **Up Arrow:** Move Up
- **Down Arrow:** Move Down
- **Left Arrow:** Move Left
- **Right Arrow:** Move Right
- **'r':** Restart the game

## Installation
1. Make sure you have Haskell installed on your system.
2. Install Gloss library for haskell using (cabal install gloss) if you're using cabal. 
3. Clone the repository:

   https://github.com/Aayush015/Web-based-game.git

After the git is cloned. 
Go to the folder where the git has been cloned. 

"cd Web-based-game"
"cd app"

Now, compile Main.hs and welcome.hs using the following commands:
"ghc --make Main.hs -o Main"
"ghc --make welcome.hs -o welcome"

Finally, run the program. 
"./welcome"
