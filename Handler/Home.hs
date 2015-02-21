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

-- HELPERS
defaultFoodPrice :: Int
defaultFoodPrice = 130

class MakeWidget a where
  makeWidget :: a -> Widget

instance MakeWidget Widget where
  makeWidget = id

instance MakeWidget Text where
  makeWidget x = toWidget [hamlet|#{x}|]

glyphicon :: Text -> Widget
glyphicon name = toWidget [hamlet|<span class="glyphicon glyphicon-#{name}">|]

linkToMethod :: MakeWidget a => Text -> Route App -> [(Text,Text)] -> a -> Widget
linkToMethod method url attributes content =
    toWidget [whamlet|
    $newline never
    <a href="@{url}" rel="nofollow" data-method="#{method}" *{attributes}>^{makeWidget content}</a>
    |]

type ConfirmText = Text
type MethodText = Text
linkToMethodConfirm :: MethodText -> ConfirmText -> Route App -> [(Text,Text)] -> Widget -> Widget
linkToMethodConfirm method confirmText route attrs
  = linkToMethod method route (("data-confirm", confirmText):attrs)

tlinkToMethodConfirm :: MethodText -> ConfirmText -> Route App -> [(Text,Text)] -> Text -> Widget
tlinkToMethodConfirm method confirmText route attrs
  = linkToMethod method route (("data-confirm", confirmText):attrs)

currentUser :: Handler (Maybe User)
currentUser = do
  maid <- maybeAuthId
  case maid of
    Just userId -> runDB $ get userId
    Nothing -> return Nothing

-- HOME
getHomeR :: Handler Html
getHomeR = do
    foods <- zip [(1::Int)..] <$> (runDB $ selectList [] [Asc FoodTitle])
    memail <- lookupSession emailSessionKey
    case memail of
      Just email -> do
        (form, _) <- generateFormPost $ orderForm (map snd foods) defaultFoodPrice Nothing email
        defaultLayout $ do
            setTitle "Czechifood"
            $(widgetFile "homepage")
      Nothing -> do
        redirect WelcomeR

data Order = Order { orderEmail :: Text, orderFood :: FoodId, orderPrice :: Int }

orderForm :: [Entity Food] -> Int -> Maybe Order -> Text -> Form Order
orderForm foods price order email = renderBootstrap3 BootstrapBasicForm $ Order
  <$> pure email
  <*> areq (selectFieldList foodList) "Jidlo" (orderFood <$> order)
  <*> pure price
  <*  bootstrapSubmit (BootstrapSubmit ("Odeslat" :: Text) "btn-default" [])
  where
    foodList = map (\(Entity key food) -> (foodTitle food, key)) foods


-- FOODS
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

-- FOOD
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


-- WELCOME
getWelcomeR :: Handler Html
getWelcomeR = do
  memail <- lookupSession emailSessionKey
  defaultLayout $(widgetFile "login")

emailSessionKey :: Text
emailSessionKey = "email"

postWelcomeR :: Handler Html
postWelcomeR = do
  email <- runInputPost $ ireq textField "email"
  setMessage . toHtml $ "Přihlášená jako: " ++ email
  setSession emailSessionKey email
  redirect HomeR

data Participant = Participant { participantEmail :: Text } deriving Show

deleteWelcomeR :: Handler Html
deleteWelcomeR = do
  deleteSession emailSessionKey
  setMessage "Odhlášení bylo úspěšné"
  redirect WelcomeR
