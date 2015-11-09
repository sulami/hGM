module Handler.Dice where

import Import

getDiceR :: Handler Html
getDiceR = do
  _ <- requireAuthId
  defaultLayout $ do
    setTitle "Dice"
    let dice = [100,20,12,10,8,6,4] :: [Int]
    $(widgetFile "dice")

