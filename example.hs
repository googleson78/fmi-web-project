{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

import Data.Aeson
import Servant.API
import Data.Proxy
import Servant.Server

newtype CounterVal = CounterVal {getCounterVal :: Int}
  deriving (Num, FromJSON, ToJSON)

type GetCounter = Get '[JSON] CounterVal
type StepCounter = "step" :> PostNoContent
type Counter = GetCounter :<|> StepCounter

counter :: Server Counter
counter = getCounter :<|> stepCounter

getCounter :: Server GetCounter
getCounter = pure $ CounterVal 5

stepCounter :: Server StepCounter
stepCounter = pure NoContent

server :: Application
server = serve (Proxy @Counter) counter
