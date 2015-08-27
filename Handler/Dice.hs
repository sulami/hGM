module Handler.Dice where

import Import

getDiceR :: Handler Html
getDiceR = defaultLayout $ do
  user <- requireAuthId
  setTitle "Dice"
  $(widgetFile "dice")
