-- | Handlers of SMS service
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Service.SMS.Handler where

import Data.Time (getCurrentTime, UTCTime)
import Web.Scotty
import FileDB.Table
import Data.Aeson (ToJSON, FromJSON)
import Control.Monad.IO.Class (liftIO)
import Service.SMS.Types (Alive (..), Message (..))
import GHC.Generics

postAliveHandler :: DataTable Alive -> ActionM ()
postAliveHandler table = do
  t <- Alive <$> liftIO getCurrentTime
  liftIO $ insert t table -- >> save table
  fmap Success (liftIO $ getAll table) >>= json

headEither :: String -> [a] -> Either String a
headEither s [] = Left s
headEither _ (x:_) = Right x

data JSONResponse a = Success { result :: a }
                    | Failure { message :: String }
                    deriving (Show, Generic)

instance (ToJSON a) => ToJSON (JSONResponse a)

mkJSONResponse :: Either String a -> JSONResponse a
mkJSONResponse (Left s) = Failure s
mkJSONResponse (Right val) = Success val

getAliveHandler :: DataTable Alive -> ActionM ()
getAliveHandler db = fmap mkJSONResponse newest >>= json
  where newest = liftIO $ getAll db .>> lastSeen .-- Desc .>> headEither "Have never seen it."

postMessageHandler :: DataTable Message -> String -> ActionM ()
postMessageHandler db msg = do
  insertMsg
  json (Success "Updated" :: JSONResponse String)
  where insertMsg = liftIO $ do
          t <- getCurrentTime
          insert (Message msg t) db >> save db

getMessageHandler :: DataTable Message -> ActionM ()
getMessageHandler db = (>>= json) . fmap Success . liftIO $ getAll db .>> timestamp .-- Desc
