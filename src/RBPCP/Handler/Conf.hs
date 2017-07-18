{-# LANGUAGE StrictData #-}
module RBPCP.Handler.Conf
( module RBPCP.Handler.Conf
, AI.AddrIndexServerUrl(..)
, module RBPCP.Internal.Manager
, module X
)
where

import           MyPrelude
import           RBPCP.Internal.Manager
import RBPCP.Handler.Internal.Blockchain          as X
import qualified Control.Monad.Reader             as Reader
import qualified Servant.Client                   as SC
import qualified PaymentChannel                   as PC
import qualified Control.Monad.Logger             as Log
import qualified Network.Bitcoin.AddrIndex.Types  as AI


data HandlerConf chanDb btcNet = HandlerConf
    { hcChanDb        :: chanDb
    , hcManager       :: ReqMan
    , hcPubKey        :: PC.RootPub
    , hcServerConf    :: ServerConf btcNet
    , hcLogLevel      :: Log.LogLevel
    }

data ServerConf btcNet = ServerConf
    { scSettings      :: PC.ServerSettings
    , scMinBtcConf    :: Word
    , scBitcoinSigner :: SC.BaseUrl
      -- | See 'BtcNetwork'. (How) do we fetch information from the Bitcoin blockchain?
    , scBlockchainCfg :: BlockchainConf btcNet
    }

--proofServerUrl = AI.getServerUrl

instance Monad m => HasReqMan (Reader.ReaderT (HandlerConf a b) m) where
    getReqMan = Reader.asks hcManager
