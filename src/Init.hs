{-# LANGUAGE TemplateHaskell #-}
module Init
( appConf
, module RBPCP.Handler.Internal.Types
)
where

import RBPCP.Handler.Internal.Types
import MyPrelude
import PaymentChannel.Types
import qualified Control.Concurrent.Cache as Cache
--
import qualified Settings
import qualified Bitcoin.SPV.Wallet           as Wall
import           Servant
import qualified Network.Wai as Wai
import qualified ChanDB                       as DB
import qualified RBPCP.Api                    as Api
import qualified Servant.Server               as SS
import qualified Control.Monad.Reader         as Reader
import qualified Control.Monad.Logger         as Log
import qualified Network.Wai.Handler.Warp     as Warp
import qualified System.IO                    as IO
import qualified System.ZMQ4                  as ZMQ

import qualified Database.Persist.Sqlite      as Sqlite




appConf :: ZMQ.Context -> IO (HandlerConf DB.DatastoreConf)
appConf ctx = do
    let notifHandler = Log.runStdoutLoggingT . $(Log.logDebugSH)
    man <- mkReqMan
    dbHandle <- DB.getHandle IO.stdout Log.LevelInfo
    pkGetter <- createPubKeyGetter dbHandle Settings.confServerExtPub
    -- Start SPV-node
    iface <- Wall.spawnWalletSimple ctx (Wall.mkConfig Wall.Prodnet Wall.LevelDebug walletDbConf) notifHandler
    return $ HandlerConf iface dbHandle pkGetter man
  where
    walletDbConf :: Sqlite.SqliteConf -- TODO: use memory?shared=1
    walletDbConf = Sqlite.SqliteConf Settings.spvWalletCacheDir 1


createPubKeyGetter ::
    ( DB.ChanDB DB.Impl handle
    ) => handle -> External ChildPub -> IO PubKeyGetter 
createPubKeyGetter dbHandle xpub = do
    void $ initializeExternalPub dbHandle xpub
    return $ Cache.fetch =<< mkCache (fmapL PubKeyDbException <$> DB.runDB dbHandle getCurrent)
  where
    cacheTimeoutSeconds = 10
    mkCache = Cache.createTimedCache (cacheTimeoutSeconds * 1000000) False 
    getCurrent :: DB.ChanDB DB.Impl dbH => DB.Impl DB.KeyAtIndex
    getCurrent = DB.pubKeyCurrent Settings.confServerExtPub

initializeExternalPub ::
    ( DB.ChanDB DB.Impl handle
    ) => handle -> External ChildPub -> IO DB.KeyAtIndex
initializeExternalPub dbHandle xpub =
    either crashWithMsg return =<< DB.runDB dbHandle pkSetup
  where
    pkSetup :: DB.ChanDB DB.Impl dbH => DB.Impl DB.KeyAtIndex
    pkSetup = DB.pubKeySetup xpub
    crashWithMsg = error . ("INIT: pubKeySetup fail: " ++) . show
