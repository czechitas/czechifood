module Handler.Welcome where

import Import
import Helpers.Common
import Yesod.Form.Bootstrap3

getWelcomeR :: Handler Html
getWelcomeR = do
  memail <- lookupSession emailSessionKey
  defaultLayout $(widgetFile "welcome")

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
