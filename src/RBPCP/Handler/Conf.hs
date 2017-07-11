{-# LANGUAGE StrictData #-}
module RBPCP.Handler.Conf
( module RBPCP.Handler.Conf
, module RBPCP.Internal.Manager
)
where

import           MyPrelude
import           RBPCP.Internal.Manager
import qualified Control.Monad.Reader         as Reader
import qualified Servant.Client               as SC
import qualified PaymentChannel               as PC
--import qualified Network.Haskoin.Constants    as HCC

data HandlerConf chanDb = HandlerConf
    { hcChanDb        :: chanDb
    -- | The server's channel public keys are derived from this key.
    --   Every time a client opens a channel, a new key is derived and handed out to new clients.
--    , hcCurrPubKey    :: PubKeyGetter
    , hcManager       :: ReqMan
    , hcPubKey        :: PC.RootPub
    , hcServerConf    :: ServerConf
    }

data ServerConf = ServerConf
    { scSettings      :: PC.ServerSettings
    , scMinBtcConf    :: Word
    , scProofServer   :: SC.BaseUrl
    , scBitcoinSigner :: SC.BaseUrl
    }

instance Monad m => HasReqMan (Reader.ReaderT (HandlerConf a) m) where
    getReqMan = Reader.asks hcManager
