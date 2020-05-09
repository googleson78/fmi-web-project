{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Example where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.IORef
import Data.Proxy
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API
import Servant.Client
import Servant.Server
import System.IO.Unsafe

newtype CounterVal = CounterVal {getCounterVal :: Int}
  deriving (Show, Num, FromJSON, ToJSON)

type Counter = GetCounter :<|> StepCounter

type GetCounter = Get '[JSON] CounterVal
type StepCounter = "step" :> Header "User" String :> ReqBody '[JSON] CounterVal :> PostNoContent

server :: Application
server = serve api counter

api :: Proxy Counter
api = Proxy

counter :: Server Counter
counter = getCounter :<|> stepCounter

getCounter :: Server GetCounter
getCounter = readCounter

stepCounter :: Server StepCounter
stepCounter muser amount =
  case muser of
    Nothing -> throwError err401 { errBody = "You must identify yourself via the \"User\" header." }
    Just user ->
      if user == "Pesho"
      then do
        incrementCounter amount
        pure NoContent
      else throwError err403 { errBody = "Only Pesho can modify the counter." }

startServer :: IO ()
startServer = do
  putStrLn "Starting a server"
  run 8080 server

incrementCounter :: CounterVal -> Handler ()
incrementCounter n = liftIO $ atomicModifyIORef' globalCounter $ \old -> (old + n, ())

readCounter :: Handler CounterVal
readCounter = liftIO $ readIORef globalCounter

globalCounter :: IORef CounterVal
globalCounter = unsafePerformIO $ newIORef $ CounterVal 0
{-# NOINLINE globalCounter #-}

reqGetCounter :: Client ClientM GetCounter
reqGetCounter = let f :<|> _ = client api in f

reqStepCounter :: Client ClientM StepCounter
reqStepCounter = let _ :<|> f = client api in f

defaultClientEnv :: ClientEnv
defaultClientEnv = unsafePerformIO $ do
  manager <- newManager defaultManagerSettings
  pure $ mkClientEnv manager $ (BaseUrl Http "localhost" 8080 "")
{-# NOINLINE defaultClientEnv #-}
