module Handler.Admin where

import Data.Map as Map hiding (foldr)
import Import hiding ((==.))
import Helpers.Common
import Database.Esqueleto as E
-- import qualified Database.Persist as P

getAdminR :: Handler Html
getAdminR = do
  orderFoods <- runDB $ select $ from $ \(o, f) -> do
              where_ (o ^. OrderFood ==. f ^. FoodId)
              return (o,f)

  let orderCount = length orderFoods
  let price = orderCount * defaultFoodPrice

  let f (_, Entity _ food) hash = Map.insertWith (+) (foodTitle food) (1 :: Int) hash
  let foodCounts = foldr f empty orderFoods

  defaultLayout $ do
      setTitle "Czechifood"
      $(widgetFile "admin")
