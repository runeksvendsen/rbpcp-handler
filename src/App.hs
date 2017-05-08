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


app :: Conf.HandlerConf ChanDB.ConfImpl
    -> Wai.Application
app conf =
    serve (api :: Proxy Api.RBPCP) $ serverEmbedConf (Server.server dbTxImpl) conf
  where
    serverEmbedConf srv cfg = enter (readerToEither cfg) srv
    api = Proxy
    dbTxImpl :: Proxy (ChanDB.TxImpl ())
    dbTxImpl = Proxy

startApp :: ZMQ.Context -> IO ()
startApp ctx = do
    let port = 8080
    conf <- Init.appConf ctx
    putStrLn $ "Starting server on port " ++ show port
    Warp.run port $ app conf

runApp :: IO ()
runApp = ZMQ.withContext startApp
