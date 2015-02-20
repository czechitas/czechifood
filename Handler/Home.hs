{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3
--  (BootstrapFormLayout (..), renderBootstrap3,
-- withSmallInput)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

class MakeWidget a where
  makeWidget :: a -> Widget

instance MakeWidget Widget where
  makeWidget = id

instance MakeWidget Text where
  makeWidget x = toWidget [hamlet|#{x}|]

glyphicon :: Text -> Widget
glyphicon name = toWidget [hamlet|<span class="glyphicon glyphicon-#{name}">|]

linkToPost = linkToMethod "POST"

linkToMethod :: MakeWidget a => Text -> Route App -> [(Text,Text)] -> a -> Widget
linkToMethod method url attributes content =
    toWidget [whamlet|
    $newline never
    <a href="@{url}" rel="nofollow" data-method="#{method}" *{attributes}>^{makeWidget content}</a>
    |]


type ConfirmText = Text
linkToMethodConfirm :: MakeWidget a =>
                       ConfirmText -> Text -> Route App -> [(Text,Text)] -> a -> Widget
linkToMethodConfirm method confirmText route attrs
  = linkToMethod method route (("data-confirm", confirmText):attrs)




getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Czechifood"
        $(widgetFile "homepage")

foodForm :: Maybe Food -> Form Food
foodForm food = renderBootstrap3 BootstrapBasicForm $ Food
            <$> areq textField (bfs ("Název" :: Text)) (foodTitle `fmap` food)
            <*> areq textField (bfs ("Popis" :: Text)) (foodDescription `fmap` food)
            <*> aopt textField (bfs ("Url obrázku" :: Text)) (foodImageUrl `fmap` food)
            <*  bootstrapSubmit (BootstrapSubmit ("Odeslat" :: Text) "btn-default" [("attribute-name","attribute-value")])

getFoodsR :: Handler Html
getFoodsR = do
  (form, _) <- generateFormPost $ foodForm Nothing
  foods <- runDB $ selectList [] [Asc FoodTitle]
  defaultLayout $(widgetFile "foods")

postFoodsR :: Handler Html
postFoodsR = do
    ((res, form),_) <- runFormPost $ foodForm Nothing
    case res of
      FormSuccess food -> do
        setMessage $ toHtml $ "Jídlo vloženo: " ++ foodTitle food
        runDB $ insert food
        return ()
      _ -> return ()

    foods <- runDB $ selectList [] [Asc FoodTitle]
    defaultLayout $(widgetFile "foods")

getFoodR :: FoodId -> Handler Html
getFoodR foodId = do
  food <- runDB $ get404 foodId
  (form, _) <- generateFormPost $ foodForm $ Just food

  defaultLayout $(widgetFile "food")

putFoodR :: FoodId -> Handler Html
putFoodR foodId = do
  food <- runDB $ get404 foodId
  ((res, form),_) <- runFormPost $ foodForm $ Just food

  case res of
    FormSuccess newFood -> do
      setMessage "Jídlo upraveno."
      runDB $ replace foodId newFood
      redirect FoodsR
    _ -> defaultLayout $(widgetFile "food")

deleteFoodR :: FoodId -> Handler Html
deleteFoodR foodId = do
  runDB $ delete foodId
  setMessage "Jidlo smazano."
  redirect FoodsR
