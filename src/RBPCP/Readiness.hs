{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module RBPCP.Readiness where

import MyPrelude
import Servant
import Bitcoin.SPV.Wallet.Types                   (SyncStatus(..))
import Control.Exception                          (throw)
import Data.Aeson                                 (FromJSON, ToJSON)
import Network.HTTP.Types.Status                  (status404)

import qualified Bitcoin.SPV.Wallet.Interface     as Wall
import qualified Servant.Client                   as SC
import qualified Servant.Server                   as SS
import qualified Network.Wai.Handler.Warp         as Warp


data ServerReadiness
  = ServerReady
  | ServerPercentDone Double
      deriving (Generic, FromJSON, ToJSON)

type Ready = "ready" :> Get '[JSON] ServerReadiness

fromSyncStatus :: SyncStatus -> ServerReadiness
fromSyncStatus (SyncDone _) = ServerReady
fromSyncStatus (SyncInProgress pct) = ServerPercentDone pct


readyServer
    :: BaseUrl
    -> Wall.Interface
    -> IO ()
readyServer url wallIface =
    Warp.run (baseUrlPort url) $ serve (Proxy :: Proxy Ready) (server wallIface)
  where
    server :: Wall.Interface -> SS.Handler ServerReadiness
    server iface = liftIO $ fromSyncStatus <$> Wall.networkSync iface

isReady :: BaseUrl -> Manager -> IO ServerReadiness
isReady url man = do
    resE <- runReq (url, man) req
    case resE of
        Right ss -> return ss
        Left e@(SC.FailureResponse status _ _) ->
            if status == status404
                then return ServerReady
                else throw e
        Left e -> throw e
  where
    req = SC.client (Proxy :: Proxy Ready)


runReq :: (BaseUrl, Manager) -> SC.ClientM a -> IO (Either SC.ServantError a)
runReq (baseUrl, man) req =
    SC.runClientM req clientEnv
  where
    clientEnv = SC.ClientEnv man baseUrl

