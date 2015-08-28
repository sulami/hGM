module Handler.EntryEdit where

import Import

import Yesod.Text.Markdown

entryForm :: UserId -> Form Entry
entryForm user = renderSemantic $ Entry
  <$> areq textField "Title" Nothing
  <*> areq markdownField "Content" Nothing
  <*> pure user

prepEntryForm :: Entry -> Form Entry
prepEntryForm entry = renderSemantic $ Entry
  <$> areq textField "Title" (Just $ entryName entry)
  <*> areq markdownField "Content" (Just $ entryContent entry)
  <*> (pure $ entryOwnerId entry)

getEntryEditR :: EntryId -> Handler Html
getEntryEditR entryId = do
  user <- requireAuthId
  entry <- runDB $ get404 entryId
  if user /= entryOwnerId entry
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else do
      (entryWidget, enctype) <- generateFormPost $ prepEntryForm entry
      defaultLayout $ do
        setTitle $ toHtml $ entryName entry
        $(widgetFile "entryedit")

postEntryEditR :: EntryId -> Handler Html
postEntryEditR entryId = do
  user <- requireAuthId
  ((res,_), _) <- runFormPost $ entryForm user
  entry <- runDB $ get404 entryId
  if user /= entryOwnerId entry
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else
      case res of
        FormSuccess entryData -> do
          runDB $ replace entryId entryData
          setMessage $ toHtml $ entryName entryData <> " saved"
          redirect $ EntriesR $ EntryR entryId
        _ -> defaultLayout $ do
          setMessage "Permission denied."
          $(widgetFile "error")

renderSemantic :: Monad m => FormRender m a
renderSemantic aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
    let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <div .ui .field :fvRequired view:.required :not $ fvRequired view:.optional>
        <label for=#{fvId view}>#{fvLabel view}
        $maybe tt <- fvTooltip view
            <div .tooltip>#{tt}
        ^{fvInput view}
        $maybe err <- fvErrors view
            <div .errors>#{err}
|]
    return (res, widget)

