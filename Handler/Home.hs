module Handler.Home where

import Import
import Helpers.Common
import Yesod.Form.Bootstrap3

orderedFoodForEmail :: Text -> Handler (Maybe Food)
orderedFoodForEmail email = do
  morder <- runDB $ selectFirst [OrderEmail ==. email] []

  case morder of
    Just order -> fmap Just $ runDB $ get404 $ orderFood $ entityVal order
    Nothing -> return Nothing


getHomeR :: Handler Html
getHomeR = do
    foods <- runDB $ selectList [] [Asc FoodTitle]
    email <- requireEmail
    mfood <- orderedFoodForEmail email

    (form, _) <- generateFormPost $ orderForm foods Nothing email
    defaultLayout $ do
        setTitle "Czechifood"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
  foods <- runDB $ selectList [] [Asc FoodTitle]
  email <- requireEmail
  mfood <- orderedFoodForEmail email

  ((res, form), _) <- runFormPost $ orderForm foods Nothing email
  case res of
    FormSuccess order -> do
      setMessage "Objednávka vytvořena."
      _ <- runDB $ insert order
      return ()
    _ -> return ()

  defaultLayout $(widgetFile "homepage")

orderForm :: [Entity Food] -> Maybe Order -> Text -> Form Order
orderForm foods order email = renderBootstrap3 BootstrapBasicForm $ Order
  <$> pure email
  <*> areq (selectFieldList foodList) "Jidlo" (orderFood <$> order)
  <*  bootstrapSubmit (BootstrapSubmit ("Odeslat" :: Text) "btn-info" [])
  where
    foodList = map (\(Entity key food) -> (foodTitle food, key)) foods
