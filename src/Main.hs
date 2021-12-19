module Main where

import Data.Either (fromLeft, isLeft)
import Data.Maybe (isJust)
import Game (Game (..), State (..), initialGame, makeMove)
import Renderer (renderBoard)

readMove :: String -> (Int, Int)
readMove (x : y : _) = (read [x] - 1, read [y] - 1)
readMove _ = error "Invalid move"

loop :: Game -> IO ()
loop game = do
  putStrLn $ renderBoard (board game)
  case state game of
    Won player -> putStrLn $ "Player " ++ show player ++ " won!"
    Draw -> putStrLn "Draw!"
    Running -> do
      putStrLn $ "Player " ++ show (player game) ++ " turn"
      putStrLn "Enter your move (e.g. 12): "
      input <- getLine
      putStrLn ""
      let (row, col) = readMove input
      let result = makeMove game row col
      case result of
        Nothing -> putStrLn "Invalid move\n" >> loop game
        Just newGame -> loop newGame

main :: IO ()
main = do
  putStrLn "Welcome to TicTacToe!\n"
  loop initialGame
