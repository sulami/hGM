module Handler.D20 where

import Import

import Game.DnD.GM.Dice

getD20R :: Handler Html
getD20R = do
  result <- liftIO $ rollIO $ d20
  sendResponse $ repPlain $ show result
