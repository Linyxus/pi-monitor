{-# LANGUAGE OverloadedStrings #-}

module Service.SMS.Route where

import Web.Scotty
import Service.SMS.Context (SMSContext (..))
import Service.SMS.Handler

routes :: SMSContext -> ScottyM ()
routes ctxt = do
  let aliveDb = aliveTable ctxt
      msgDb = msgTable ctxt
  get "/sms/hello" $ text "Hello"
  post "/sms/alive" $ postAliveHandler aliveDb
  get "/sms/alive" $ getAliveHandler aliveDb
  post "/sms/message" $ do
    msg <- param "msg"
    postMessageHandler msgDb msg
  get "/sms/message" $ getMessageHandler msgDb
