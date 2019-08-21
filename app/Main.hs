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

import Network.Wai (requestMethod, Request)
import Network.HTTP.Types.Method

isNotOptionReq :: Request -> IO Bool
isNotOptionReq req = return $ requestMethod req /= methodOptions

-- corsPolicy :: CorsResourcePolicy
-- corsPolicy = CorsResourcePolicy { corsOrigins = Nothing
--                                 , corsMethods = [ methodGet, methodPost
--                                                 , methodHead, methodOption ]}

main :: IO ()
main = do
  smsCtxt <- SMSC.initialize
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ basicAuth (\_ _ -> return True) "My Realm" { authIsProtected = isNotOptionReq }
    -- middleware $ cors (const . pure $ corsPolicy )
    options (regex "^(.*)$") $ do
      liftIO $ putStrLn "Options! Catch you!"
      addHeader "Access-Control-Allow-Origin" "*"
      addHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS"
      addHeader "Access-Control-Allow-Headers" "*"
    SMSR.routes smsCtxt
