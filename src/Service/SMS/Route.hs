{-# LANGUAGE OverloadedStrings #-}

module Service.SMS.Route where

import Web.Scotty
import Service.SMS.Context (SMSContext (..))
import Service.SMS.Handler

routes :: SMSContext -> ScottyM ()
routes ctxt = do
  let aliveDb = aliveTable ctxt
      msgDb = msgTable ctxt
  get "/api/sms/hello" $ text "Hello"
  post "/api/sms/alive" $ postAliveHandler aliveDb
  get "/api/sms/alive" $ getAliveHandler aliveDb
  post "/api/sms/message" $ do
    msg <- param "msg"
    postMessageHandler msgDb msg
  get "/api/sms/message" $ getMessageHandler msgDb
