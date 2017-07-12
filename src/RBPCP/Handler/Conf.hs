{-# LANGUAGE StrictData #-}
module RBPCP.Handler.Conf
( module RBPCP.Handler.Conf
, AI.AddrIndexServerUrl(..)
, module RBPCP.Internal.Manager
)
where

import           MyPrelude
import           RBPCP.Internal.Manager
import qualified Control.Monad.Reader         as Reader
import qualified Servant.Client               as SC
import qualified PaymentChannel               as PC
import qualified Control.Monad.Logger         as Log
import qualified Network.Bitcoin.AddrIndex.Types as AI


data HandlerConf chanDb = HandlerConf
    { hcChanDb        :: chanDb
    -- | The server's channel public keys are derived from this key.
    --   Every time a client opens a channel, a new key is derived and handed out to new clients.
--    , hcCurrPubKey    :: PubKeyGetter
    , hcManager       :: ReqMan
    , hcPubKey        :: PC.RootPub
    , hcServerConf    :: ServerConf
    , hcLogLevel      :: Log.LogLevel
    }

data ServerConf = ServerConf
    { scSettings      :: PC.ServerSettings
    , scMinBtcConf    :: Word
--    , scMinTxFee      :: PC.BtcAmount
    , scProofServer   :: AI.AddrIndexServerUrl
    , scBitcoinSigner :: SC.BaseUrl
    }

proofServerUrl = AI.getServerUrl

instance Monad m => HasReqMan (Reader.ReaderT (HandlerConf a) m) where
    getReqMan = Reader.asks hcManager
