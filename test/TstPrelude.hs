module TstPrelude
( module TstPrelude
, HTTP.Manager
)
where

import qualified Servant.Client             as SC
import Servant.Common.BaseUrl
import qualified Network.HTTP.Client        as HTTP
import Control.Exception                    (throw)

runReq :: (BaseUrl, HTTP.Manager) -> SC.ClientM a -> IO (Either SC.ServantError a)
runReq (baseUrl, man) req =
    SC.runClientM req clientEnv
  where
    clientEnv = SC.ClientEnv man baseUrl

testRunReq :: (BaseUrl, HTTP.Manager) -> SC.ClientM a -> IO a
testRunReq (baseUrl, man) req =
    either throw return =<< runReq (baseUrl, man) req

mkManager = HTTP.newManager HTTP.defaultManagerSettings

--runHandler :: ExceptT.ExceptT SS.ServantErr IO a -> IO (Either String a)
--runHandler = fmap (fmapL show) . ExceptT.runExceptT