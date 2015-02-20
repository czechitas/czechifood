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
getHomeR :: Handler Html
getHomeR = do
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
