{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Helpers.Common where

import Import
import Control.Monad

emailSessionKey :: Text
emailSessionKey = "email"

adminSessionKey :: Text
adminSessionKey = "password"

defaultFoodPrice :: Int
defaultFoodPrice = 150


requireAdmin action = do
  isLogged <- isAdmin
  if isLogged
    then action
    else redirect AdminLoginR

isAdmin :: MonadHandler m => m Bool
isAdmin = do
  mpassword <- lookupSession adminSessionKey
  case mpassword of
    Just password -> return True
    Nothing -> return False

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

requireEmail :: Handler Text
requireEmail = do
  memail <- lookupSession emailSessionKey
  case memail of
    Just email -> return email
    Nothing -> do
      setMessage "Musite se prihlasit"
      redirect WelcomeR
