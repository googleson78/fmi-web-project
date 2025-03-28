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

api :: Proxy Counter
api = Proxy

counter :: Server Counter
counter = getCounter :<|> stepCounter

-- "real" signature:
-- getCounter :: Handler CounterVal
getCounter :: Server GetCounter
getCounter = readCounter

-- "real" signature:
-- stepCounter :: Maybe String -> CounterVal -> Handler NoContent
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
  run 8080 (serve api counter)

incrementCounter :: CounterVal -> Handler ()
incrementCounter n = liftIO $ atomicModifyIORef' globalCounter $ \old -> (old + n, ())

readCounter :: Handler CounterVal
readCounter = liftIO $ readIORef globalCounter

-- the NOINLINE here and in defaultClientEnv
-- is a popular haskell trick to make "global" variables
-- it ensures that the compiler will not try to inline this value in multiple places and evaluate it more than once
globalCounter :: IORef CounterVal
globalCounter = unsafePerformIO $ newIORef $ CounterVal 0
{-# NOINLINE globalCounter #-}

-- "real" signature:
-- reqGetCounter :: ClientM CounterVal
reqGetCounter :: Client ClientM GetCounter
reqGetCounter = let f :<|> _ = client api in f

-- "real" signature:
-- reqStepCounter :: Maybe String -> CounterVal -> ClientM NoContent
reqStepCounter :: Client ClientM StepCounter
reqStepCounter = let _ :<|> f = client api in f

defaultClientEnv :: ClientEnv
defaultClientEnv = unsafePerformIO $ do
  manager <- newManager defaultManagerSettings
  pure $ mkClientEnv manager $ (BaseUrl Http "localhost" 8080 "")
{-# NOINLINE defaultClientEnv #-}
