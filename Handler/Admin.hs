module Handler.Admin where

import Data.Map as Map hiding (foldr)
import Import hiding ((==.))
import Helpers.Common
import Database.Esqueleto as E
-- import qualified Database.Persist as P

getAdminR :: Handler Html
getAdminR = requireAdmin $ do
  orderFoods <- runDB $ select $ from $ \(o, f) -> do
              where_ (o ^. OrderFood ==. f ^. FoodId)
              return (o,f)

  let orderCount = length orderFoods
  priceRef <- fmap price getYesod
  defaultFoodPrice <- liftIO $ readIORef priceRef

  let price = orderCount * defaultFoodPrice

  let f (_, Entity _ food) hash = Map.insertWith (+) (foodTitle food) (1 :: Int) hash
  let foodCounts = foldr f empty orderFoods

  defaultLayout $ do
      setTitle "Czechifood"
      $(widgetFile "admin")

deleteOrderR :: OrderId -> Handler Html
deleteOrderR orderId = do
  runDB $ Import.delete orderId
  setMessage "Objednávka smazána."
  redirect AdminR

deleteOrdersR :: Handler Html
deleteOrdersR = do
  runDB $ deleteWhere ([] :: [Filter Order])
  setMessage "Objednávky smazány."
  redirect AdminR

getAdminLoginR :: Handler Html
getAdminLoginR = do
  adminLogged <- isAdmin
  defaultLayout $(widgetFile "admin-login")

postAdminLoginR :: Handler Html
postAdminLoginR = do
  password <- runInputPost $ ireq textField "password"
  if password == "ditamija"
    then do
      setSession adminSessionKey password
      setMessage "Přihlášená jako admin"
    else return ()
  redirect AdminLoginR

deleteAdminLoginR :: Handler Html
deleteAdminLoginR = do
  deleteSession adminSessionKey
  setMessage "Odhlášení bylo úspěšné"
  redirect AdminLoginR
