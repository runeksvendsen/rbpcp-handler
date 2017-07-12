{-# LANGUAGE TemplateHaskell #-}
module RBPCP.Internal.Init
( appConf
, module RBPCP.Handler.Internal.Types
)
where

import RBPCP.Handler.Internal.Types
import MyPrelude
import PaymentChannel.Types
import Servant
import qualified Servant.Client               as SC
import qualified RBPCP.Internal.Manager       as Man
import qualified Control.Concurrent.Cache     as Cache
import qualified ChanDB                       as DB
import qualified Control.Monad.Logger         as Log
import qualified System.IO                    as IO
import qualified BitcoinSigner.Lib.Signing    as Sign


fetchXPub :: SC.ClientM RootPub
fetchXPub :<|> _ :<|> _ :<|> _ = SC.client api
    where api :: Proxy Sign.BTCSign
          api  = Proxy

appConf
    :: forall txM dbM handle. DB.ChanDBTx txM dbM handle
    => Proxy (txM ())           -- ^ Database implementation
    -> Log.LogLevel
    -> ServerConf
    -> IO (HandlerConf handle)
appConf dbImpl logLvl cfg = do
    man <- mkReqMan
    dbHandle <- DB.getHandle IO.stdout logLvl
    serverXPub <- either (\e -> error $ "Failed to fetch XPub from bitcoin-signer: " ++ show e) return =<<
        runServantClient man (scBitcoinSigner cfg) fetchXPub
    -- pkGetter <- createPubKeyGetter dbImpl (dbHandle :: handle) serverXPub
    return $ HandlerConf dbHandle man serverXPub cfg logLvl

-- createPubKeyGetter = undefined

{-
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
    getCurrent = DB.pubKeyCurrent xpub

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
-}