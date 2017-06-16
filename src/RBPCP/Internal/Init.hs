{-# LANGUAGE TemplateHaskell #-}
module RBPCP.Internal.Init
( appConf
, module RBPCP.Handler.Internal.Types
)
where

import RBPCP.Handler.Internal.Types
import MyPrelude
import PaymentChannel.Types
import qualified                                 Settings
import qualified Control.Concurrent.Cache     as Cache
import qualified Bitcoin.SPV.Wallet           as Wall
import qualified ChanDB                       as DB
import qualified Control.Monad.Logger         as Log
import qualified System.IO                    as IO
import qualified System.ZMQ4                  as ZMQ
import qualified Network.Haskoin.Constants    as HCC
import qualified Database.Persist.Sqlite      as Sqlite


appConf
    :: forall txM dbM handle. DB.ChanDBTx txM dbM handle
    => Proxy (txM ())           -- ^ Database implementation
    -> ZMQ.Context
    -> IO (HandlerConf handle)
appConf dbImpl ctx = do
    man <- mkReqMan
    dbHandle <- DB.getHandle IO.stdout Log.LevelInfo
    pkGetter <- createPubKeyGetter dbImpl (dbHandle :: handle) Settings.confServerExtPub
    return $ HandlerConf dbHandle pkGetter man

--    let notifHandler = Log.runStdoutLoggingT . $(Log.logDebugSH)
--        btcNet = if HCC.getNetwork == HCC.prodnet then Wall.Prodnet else Wall.Testnet
--    iface <- Wall.spawnWalletSimple ctx (Wall.mkConfig btcNet Wall.LevelDebug walletDbConf) notifHandler
--  where
--    walletDbConf :: Sqlite.SqliteConf -- TODO: use memory?shared=1
--    walletDbConf = Sqlite.SqliteConf Settings.confSpvWalletCacheDir 1

createPubKeyGetter
    :: forall txM dbM handle. DB.ChanDBTx txM dbM handle
    => Proxy (txM ())           -- ^ Database implementation
    -> handle
    -> External ChildPub
    -> IO PubKeyGetter
createPubKeyGetter dbImpl dbHandle xpub = do
    void $ initializeExternalPub dbImpl dbHandle xpub
    return $ Cache.fetch =<< mkCache (fmapL PubKeyDbException <$> DB.runDB dbHandle getCurrent)
  where
    cacheTimeoutSeconds = 10
    mkCache = Cache.createTimedCache (cacheTimeoutSeconds * 1000000) False 
    getCurrent :: dbM DB.KeyAtIndex
    getCurrent = DB.pubKeyCurrent Settings.confServerExtPub

initializeExternalPub
    :: forall txM dbM handle. DB.ChanDBTx txM dbM handle
    => Proxy (txM ())           -- ^ Database implementation
    -> handle
    -> External ChildPub
    -> IO DB.KeyAtIndex
initializeExternalPub _ dbHandle xpub =
    either crashWithMsg return =<< DB.runDB dbHandle pkSetup
  where
    pkSetup :: dbM DB.KeyAtIndex
    pkSetup = DB.pubKeySetup xpub
    crashWithMsg = error . ("INIT: pubKeySetup fail: " ++) . show
