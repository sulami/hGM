module Handler.DiceRoll where

import Import

import Game.DnD.GM.Dice

getDiceRollR :: Int -> Int -> Handler Html
getDiceRollR num sides = do
  result <- liftIO $ roll1IO roll sides
  sendResponse $ repPlain $ show result

