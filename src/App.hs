-- {-# LANGUAGE ScopedTypeVariables   #-}
module App where

import MyPrelude
import Servant
import qualified Conf
import qualified Init
import qualified ChanDB
import qualified RBPCP.Server                 as Server
import qualified Network.Wai                  as Wai
import qualified RBPCP.Api                    as Api
import qualified Network.Wai.Handler.Warp     as Warp
import qualified System.ZMQ4                  as ZMQ


-- |
createApp :: ChanDB.ChanDBTx txM dbM dbH
          => Proxy (txM ())           -- ^ Database implementation
          -> Conf.HandlerConf dbH
          -> Server.PaymentCallback
          -> Wai.Application
createApp dbImpl conf cb =
    serve (Proxy :: Proxy Api.RBPCP) $ serverEmbedConf (Server.createServer dbImpl cb) conf
  where
    serverEmbedConf srv cfg = enter (readerToEither cfg) srv

testApp :: ZMQ.Context -> IO ()
testApp ctx = do
    let port = 8080
        datastoreDb :: Proxy (ChanDB.TxImpl ())
        datastoreDb = Proxy
        noOpCallback = Server.PaymentCallback $ \_ _ _ -> return (Server.CallbackResult $ Right "")
    conf <- Init.appConf ctx
    putStrLn $ "Starting server on port " ++ show port
    Warp.run port $ createApp datastoreDb conf noOpCallback

runTestApp :: IO ()
runTestApp = ZMQ.withContext testApp
