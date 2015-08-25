module Handler.EntryEdit where

import Import

entryForm :: Form Entry
entryForm = renderSemantic $ Entry
  <$> areq textField "Title" Nothing
  <*> areq textareaField "Content" Nothing

prepEntryForm :: Entry -> Form Entry
prepEntryForm entry = renderSemantic $ Entry
  <$> areq textField "Title" (Just $ entryName entry)
  <*> areq textareaField "Content" (Just $ entryContent entry)

getEntryEditR :: EntryId -> Handler Html
getEntryEditR entryId = do
  entry <- runDB $ get404 entryId
  (entryWidget, enctype) <- generateFormPost $ prepEntryForm entry
  defaultLayout $ do
    setTitle $ toHtml $ entryName $ entry
    addStylesheetRemote "//cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.0.8/semantic.min.css"
    $(widgetFile "entryedit")

postEntryEditR :: EntryId -> Handler Html
postEntryEditR entryId = do
  ((res,entryWidget), enctype) <- runFormPost entryForm
  entry <- runDB $ get404 entryId
  case res of
    FormSuccess entryData -> do
      runDB $ replace entryId entryData
      setMessage $ toHtml $ (entryName entry) <> " created"
      redirect $ EntryR entryId
    _ -> defaultLayout $ do
      setTitle "Bad."
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

