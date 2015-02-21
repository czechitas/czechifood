module Handler.Food where

import Import
import Helpers.Common
import Yesod.Form.Bootstrap3

foodForm :: Maybe Food -> Form Food
foodForm food = renderBootstrap3 BootstrapBasicForm $ Food
            <$> areq textField (bfs ("Název" :: Text)) (foodTitle `fmap` food)
            <*> areq textField (bfs ("Popis" :: Text)) (foodDescription `fmap` food)
            <*> aopt textField (bfs ("Url obrázku" :: Text)) (foodImageUrl `fmap` food)
            <*  bootstrapSubmit (BootstrapSubmit ("Odeslat" :: Text) "btn-default" [])

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
        _ <- runDB $ insert food
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
  setMessage "Jídlo smazáno."
  redirect FoodsR
