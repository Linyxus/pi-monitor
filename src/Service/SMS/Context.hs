-- | Context for SMS service

module Service.SMS.Context where

import Service.SMS.Types
import FileDB.Table

data SMSContext = SMSContext { aliveTable :: DataTable Alive
                             , msgTable :: DataTable Message
                             }

initialize :: IO SMSContext
initialize = SMSContext <$>
             createTable "./db/sms.alive.db" ([] :: [Alive]) <*>
             createTable "./db/sms.msg.db" ([] :: [Message])
