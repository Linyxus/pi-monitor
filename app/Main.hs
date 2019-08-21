{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.HttpAuth
import Network.Wai.Middleware.Cors
import Control.Monad.IO.Class (liftIO)

import Service.SMS.Route as SMSR
import Service.SMS.Context as SMSC

main :: IO ()
main = do
  smsCtxt <- SMSC.initialize
  scotty 3000 $ do
    middleware simpleCors
    middleware logStdoutDev
    middleware $ basicAuth (\u p -> return True) "My Realm"
    SMSR.routes smsCtxt
