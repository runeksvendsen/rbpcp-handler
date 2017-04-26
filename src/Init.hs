module Init
( createPubKeyGetter
, module RBPCP.Handler.Internal.Types
)
where

import RBPCP.Handler.Internal.Types
import MyPrelude
import PaymentChannel.Types
import qualified Settings
import qualified ChanDB         as DB
import qualified Servant.Server as SS
import qualified Control.Concurrent.Cache as Cache


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
