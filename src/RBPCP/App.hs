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
createApp :: forall txM dbM dbH chain chainM.
             ( ChanDB.ChanDBTx txM dbM dbH
             , Conf.BlockchainRun chainM chain
             )
          => Proxy (txM ())           -- ^ Database implementation
          -> Conf.HandlerConf dbH chain
          -> Server.PaymentCallback
          -> Wai.Application
createApp _ conf cb =
    serve (Proxy :: Proxy Api.RBPCP) $ serverEmbedConf
        (Server.createServer dbChainImpl cb :: ServerT Api.RBPCP (Init.HandlerM dbH chain)) conf
  where
    dbChainImpl :: Proxy (txM (),chain)
    dbChainImpl = Proxy
    serverEmbedConf srv cfg = enter (readerToEither cfg) srv

testApp :: forall chain chainM. Conf.BlockchainRun chainM chain
        => Log.LogLevel
        -> Word
        -> Conf.ServerConf chain
        -> IO ()
testApp logLvl port servConf = do
    let datastoreDb :: Proxy (ChanDB.TxImpl ())
        datastoreDb = Proxy
        noOpCallback = Server.PaymentCallback $ \_ _ _ -> return (Server.CallbackResult $ Right "")
    conf <- Init.appConf datastoreDb logLvl servConf
    putStrLn $ "Starting server on port " ++ show port
    Warp.run (fromIntegral port) $ createApp datastoreDb conf noOpCallback
