module Handler.Faq where

import           Import

getFaqR :: Handler Html
getFaqR = defaultLayout $ do
  setTitle "FAQ"
  $(widgetFile "faq")

