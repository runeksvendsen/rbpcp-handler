module RBPCP.Internal.Manager where

import qualified Servant.Client               as SC
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad.Reader


newtype HTTP a = HTTP a
newtype HTTPS a = HTTPS a

class IsManager a where
    mkMan  :: IO a

instance IsManager (HTTP Manager) where
    mkMan  = HTTP <$> newManager defaultManagerSettings

instance IsManager (HTTPS Manager) where
    mkMan  = HTTPS <$> newManager tlsManagerSettings

data ReqMan = ReqMan (HTTP Manager) (HTTPS Manager)

mkReqMan :: IO ReqMan
mkReqMan = ReqMan <$> mkMan <*> mkMan


runClientM
    :: ReqMan
    -> SC.BaseUrl
    -> SC.ClientM a
    -> IO (Either SC.ServantError a)
runClientM (ReqMan (HTTP httpMan) (HTTPS tlsMan)) url req = do
    let man = case SC.baseUrlScheme url of
          SC.Http  -> httpMan
          SC.Https -> tlsMan
    SC.runClientM req (clientEnv man)
  where
    clientEnv m = SC.ClientEnv m url

class HasReqMan m where
    getReqMan :: m ReqMan

handlerReq ::
       (MonadIO m, HasReqMan m)
    => SC.BaseUrl
    -> SC.ClientM a
    -> m (Either SC.ServantError a)
handlerReq url req = do
    man <- getReqMan
    liftIO $ runClientM man url req
