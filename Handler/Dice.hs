module Handler.Dice where

import Import

getDiceR :: Handler Html
getDiceR = do
  _ <- requireAuthId
  defaultLayout $ do
    setTitle "Dice"
    $(widgetFile "dice")

