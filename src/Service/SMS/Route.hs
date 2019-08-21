{-# LANGUAGE OverloadedStrings #-}

module Service.SMS.Route where

import Web.Scotty
import Service.SMS.Context (SMSContext (..))
import Service.SMS.Handler
import Control.Monad.IO.Class (liftIO)

corsHeader = do
  addHeader "Access-Control-Allow-Origin" "*"

routes :: SMSContext -> ScottyM ()
routes ctxt = do
  let aliveDb = aliveTable ctxt
      msgDb = msgTable ctxt
  get "/api/sms/hello" $ text "Hello"
  post "/api/sms/alive" $ postAliveHandler aliveDb >> corsHeader
  get "/api/sms/alive" $ getAliveHandler aliveDb >> corsHeader
  post "/api/sms/message" $ do
    msg <- param "msg"
    postMessageHandler msgDb msg >> corsHeader
  get "/api/sms/message" $ getMessageHandler msgDb >> corsHeader
