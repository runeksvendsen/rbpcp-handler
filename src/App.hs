{-# LANGUAGE TypeOperators, TemplateHaskell, ScopedTypeVariables #-}
module App where

import MyPrelude
import RBPCP.Handler.Internal.Util
import qualified Settings
import qualified Conf
import qualified Init
import qualified RBPCP.Server                 as Server
import qualified Bitcoin.SPV.Wallet           as Wall
import qualified RBPCP.Types                  as RBPCP
import           Servant
import qualified Network.Wai as Wai
import qualified ChanDB                       as DB
import qualified RBPCP.Api                    as Api
import qualified Servant.Server               as SS
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Logger as Log
import qualified Network.Wai.Handler.Warp   as Warp
import qualified System.IO as IO
import qualified System.ZMQ4                  as ZMQ

import qualified Database.Persist.Sqlite      as Sqlite



server :: ServerT Api.RBPCP (HandlerM DB.ConfImpl)
server =
    Server.fundingInfo :<|> open :<|> pay :<|> close
  where
    close tid vout s p = Server.runClose
        (Server.closeE tid vout s p :: EitherT (HandlerErr PayChanError) DB.TxImpl (RBPCP.PaymentResult, BitcoinTx))
    pay   tid vout s p = runAtomic
        (Server.payE tid vout s p  :: EitherT (HandlerErr PayChanError) DB.TxImpl RBPCP.PaymentResult)
    open  tid vout s p = do
        infoM <- Server.getConfProof tid
        let openET = Server.openE infoM tid vout s p
        Server.runOpen (openET :: EitherT (HandlerErr Server.OpenErr) DB.Impl RBPCP.PaymentResult)



app :: Conf.HandlerConf DB.DatastoreConf
    -> Wai.Application
app conf =
    serve (api :: Proxy Api.RBPCP) $ serverEmbedConf server conf
  where
    serverEmbedConf srv cfg = enter (readerToEither cfg) srv
    api = Proxy

startApp :: ZMQ.Context -> IO ()
startApp ctx = do
    let notifHandler = Log.runStdoutLoggingT . $(Log.logDebugSH)
        port = 8080
    dbHandle <- DB.getHandle IO.stdout Log.LevelInfo
    pkGetter <- Init.createPubKeyGetter dbHandle Settings.confServerExtPub
    man      <- newTlsManager
    -- Start SPV-node
    iface <- Wall.spawnWalletSimple ctx (Wall.mkConfig Wall.Prodnet Wall.LevelDebug walletDbConf) notifHandler
    -- Start server
    putStrLn $ "Starting server on port " ++ show port
    Warp.run port $ app (Conf.HandlerConf iface dbHandle pkGetter man)
  where
    walletDbConf :: Sqlite.SqliteConf -- TODO: use memory?shared=1
    walletDbConf = Sqlite.SqliteConf Settings.spvWalletCacheDir 1

runApp = ZMQ.withContext startApp
