{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


module Api where

import Prelude ()
import Prelude.Compat


import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

data HTMLLucid

instance Accept HTMLLucid where
  contentType _ = "text" // "html"

instance ToHtml a => MimeRender HTMLLucid a where
  mimeRender _ = renderBS . toHtml

type UserAPI1 = "users" :> Get '[JSON, HTMLLucid] [User] :<|> Capture "name" String :> Get '[JSON, HTMLLucid] User

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

instance ToHtml User where
  toHtml person =
    tr_ $ do
      let userFns = [name, show.age, email, show.registration_date]
      foldMap (\fn -> td_ $ toHtml $ fn person) userFns
      
  toHtmlRaw = toHtml

instance ToHtml [User] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "Name"
      th_ "Age"
      th_ "Email"
      th_ "Registration Date"
      foldMap toHtml persons

  toHtmlRaw = toHtml

lowerString :: String -> String
lowerString = fmap toLower

users1 :: [User]
users1 = [albert, isaac]

usersMap :: Map.Map String User
usersMap = Map.fromList $ map (\user -> ((firstName $ name user), user)) users1
           where
             firstName :: String -> String
             firstName = lowerString.head.words

albert :: User
albert =  User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)

isaac :: User
isaac =  User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)

server1 :: Server UserAPI1
server1 = return users1 :<|> findUser
          where
            findUser :: String -> Handler User
            findUser str = return $
              case (usersMap Map.!? str) of
                Just user -> user
                Nothing -> User "nul" 0 "nul" (fromGregorian 0 0 0)

userAPI :: Proxy UserAPI1
userAPI = Proxy

type APIFor a i =
  Get '[JSON] [a] -- list 'a's
  :<|> ReqBody '[JSON] a :> PostNoContent '[JSON] NoContent -- add an 'a'
  :<|> Capture "id" i :>
         ( Get '[JSON] a -- view an 'a' given its "identifier" of type 'i'
      :<|> ReqBody '[JSON] a :> PutNoContent '[JSON] NoContent -- update an 'a'
      :<|> DeleteNoContent '[JSON] NoContent -- delete an 'a'
         )
         
serverFor :: forall a i. Handler [a] -- handler for listing of 'a's
          -> (a -> Handler NoContent) -- handler for adding an 'a'
          -> (i -> Handler a) -- handler for viewing an 'a' given its identifier of type 'i'
          -> (i -> a -> Handler NoContent) -- updating an 'a' with given id
          -> (i -> Handler NoContent) -- deleting an 'a' given its id
          -> Server (APIFor a i)
          
serverFor getAs addA viewA updateA deleteA =
  getAs :<|> addA :<|>  aOperations
    where
      aOperations id = viewA id :<|> updateA id :<|> deleteA id
  
app1 :: Application
app1 = serve userAPI server1

runApp :: IO ()
runApp = run 8081 app1
