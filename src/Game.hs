module Game where

import Data.List (transpose)
import Data.Maybe (isJust, isNothing)

data Player = PlayerX | PlayerO deriving (Eq)

instance Show Player where
  show PlayerX = "X"
  show PlayerO = "O"

type Board = [[Maybe Player]]

data State = Running | Won Player | Draw deriving (Eq, Show)

data Game = Game
  { board :: Board,
    player :: Player,
    state :: State
  }
  deriving (Eq, Show)

initialBoard :: Board
initialBoard = replicate 3 $ replicate 3 Nothing

initialGame :: Game
initialGame = Game initialBoard PlayerX Running

makeMove :: Game -> Int -> Int -> Maybe Game
makeMove game row col
  | isEmpty = Just $ Game newBoard newPlayer newState
  | otherwise = Nothing
  where
    isEmpty = isNothing (board game !! row !! col)
    newBoard = updateBoard (board game) row col (player game)
    newPlayer = nextPlayer (player game)
    newState = checkState newBoard

updateBoard :: Board -> Int -> Int -> Player -> Board
updateBoard board row col player = replace board row (replace (board !! row) col (Just player))

replace :: [a] -> Int -> a -> [a]
replace xs n new = take n xs ++ [new] ++ drop (n + 1) xs

nextPlayer :: Player -> Player
nextPlayer PlayerX = PlayerO
nextPlayer PlayerO = PlayerX

checkState :: Board -> State
checkState board
  | isWon board PlayerX = Won PlayerX
  | isWon board PlayerO = Won PlayerO
  | isDraw board = Draw
  | otherwise = Running

isWon :: Board -> Player -> Bool
isWon board player =
  let rows = board
      cols = transpose rows
      diag1 = [rows !! i !! i | i <- [0 .. 2]]
      diag2 = [rows !! i !! (2 - i) | i <- [0 .. 2]]
   in any (all (== Just player)) (rows ++ cols ++ [diag1, diag2])

isDraw :: Board -> Bool
isDraw board = all (all isJust) board && not (isWon board PlayerX || isWon board PlayerO)
