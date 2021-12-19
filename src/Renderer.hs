module Renderer where

import Game (Board, Player (..))

renderPlayer :: Maybe Player -> String
renderPlayer (Just player) = show player
renderPlayer Nothing = "□"

renderBoard :: Board -> String
renderBoard board = unlines $ map (unwords . map renderPlayer) board
