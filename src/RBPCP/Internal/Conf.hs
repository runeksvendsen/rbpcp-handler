module RBPCP.Internal.Conf
( module RBPCP.Internal.Conf
, module RBPCP.Internal.Manager
)
where

import           MyPrelude
import           RBPCP.Internal.Manager
import qualified Bitcoin.SPV.Wallet.Interface     as Wall
import qualified Control.Monad.Reader             as Reader


data HandlerConf chanDb = HandlerConf
    { hcChanDb      :: !chanDb
    , hcCurrPubKey  :: !PubKeyGetter
    , hcManager     :: !ReqMan
    }

instance Monad m => HasReqMan (Reader.ReaderT (HandlerConf a) m) where
    getReqMan = Reader.asks hcManager

