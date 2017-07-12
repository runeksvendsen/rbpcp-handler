-- {-# LANGUAGE ScopedTypeVariables   #-}
module RBPCP.App where

import MyPrelude
import Servant
import qualified RBPCP.Internal.Init          as Init
import qualified                                 ChanDB
import qualified RBPCP.Handler.Conf          as Conf
import qualified RBPCP.Server                 as Server
import qualified Network.Wai                  as Wai
import qualified RBPCP.Api                    as Api
import qualified Network.Wai.Handler.Warp     as Warp
import qualified Control.Monad.Logger                 as Log


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

testApp :: Log.LogLevel -> Word -> Conf.ServerConf -> IO ()
testApp logLvl port servConf = do
    let datastoreDb :: Proxy (ChanDB.TxImpl ())
        datastoreDb = Proxy
        noOpCallback = Server.PaymentCallback $ \_ _ _ -> return (Server.CallbackResult $ Right "")
    conf <- Init.appConf datastoreDb logLvl servConf
    putStrLn $ "Starting server on port " ++ show port
    Warp.run (fromIntegral port) $ createApp datastoreDb conf noOpCallback
