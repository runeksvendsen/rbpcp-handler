module RBPCP.Handler.Internal.Types
( module RBPCP.Handler.Internal.Types
, module RBPCP.Handler.Internal.Error
, module RBPCP.Handler.Conf
)
where

import MyPrelude
import RBPCP.Handler.Internal.Error
import RBPCP.Handler.Conf
import qualified ChanDB         as DB

import qualified Control.Monad.Reader         as Reader
import qualified Servant.Server               as SS
import qualified PaymentChannel               as PC


type HandlerM dbConf = AppM (HandlerConf dbConf)


class HasAppConf m dbH where
    getAppConf :: m (HandlerConf dbH)

instance HasAppConf (HandlerM dbH) dbH where
    getAppConf = Reader.ask


--class HasSpvWallet m where
--    wallIface :: m Wall.Interface
--
--instance Monad m => HasSpvWallet (ReaderT (HandlerConf dbH) m) where
--    wallIface = Reader.asks hcSpvWallet


class DB.ChanDB dbM dbH => HasDb m dbM dbH where
    liftDB :: dbM a -> m a

instance DB.ChanDB dbM dbH => HasDb (ReaderT (HandlerConf dbH) dbM) dbM dbH where
    liftDB = lift

instance DB.ChanDB dbM dbH => HasDb (ReaderT (HandlerConf dbH) (EitherT (HandlerErr e) dbM)) dbM dbH where
    liftDB = lift . lift


class DB.ChanDBTx dbTxM dbM dbH => HasDbTx m dbTxM dbM dbH where
    liftDbTx :: dbTxM a -> m a

instance (DB.ChanDBTx dbTxM dbM dbH, Monad dbTxM)
            => HasDbTx (ReaderT (HandlerConf dbH) dbTxM) dbTxM dbM dbH where
    liftDbTx = lift

instance (DB.ChanDBTx dbTxM dbM dbH, Monad dbTxM)
            => HasDbTx (ReaderT (HandlerConf dbH) (EitherT (HandlerErr e) dbTxM)) dbTxM dbM dbH where
    liftDbTx = lift . lift



type BlockNumber = Word32


data HandlerErr a
  = HandlerErr a
  | UserError UserError
  | InternalErr InternalError
      deriving (Eq, Show)

toHandlerEx :: DB.ChanDBException -> HandlerErr a
toHandlerEx e =
        if DB.is404 e
            then UserError ResourceNotFound
            else InternalErr $ OtherInternalErr "DB Error"

class Show e => IsHandlerException e where
    mkHandlerErr :: e -> SS.ServantErr

instance IsHandlerException a => IsHandlerException (HandlerErr a) where
    mkHandlerErr (HandlerErr e) = mkHandlerErr e
    mkHandlerErr (InternalErr _) = mkServantErr SS.err500 (OtherInternalErr "")
    mkHandlerErr (UserError e) = mkHandlerErr e

instance IsHandlerException DB.ChanDBException where
    mkHandlerErr e =
        if DB.is404 e
            then SS.err404
            else mkServantErr SS.err500 (OtherInternalErr "")

instance IsHandlerException PC.PayChanError where
    mkHandlerErr = mkServantErr SS.err400

--instance IsHandlerException PubKeyDbException where
--    mkHandlerErr _ = mkServantErr SS.err500 (OtherInternalErr "")

instance IsHandlerException UserError where
    mkHandlerErr ResourceNotFound = SS.err404
    mkHandlerErr (ResourcePaymentMismatch txid vout s) = SS.err301
         { SS.errHeaders = [ ("Location", cs $ mkUrl txid vout s) ]
         , SS.errHTTPCode = 308   -- repeat request of same type (POST/PUT/etc.) on new URL
         }

instance IsHandlerException InternalError where
    mkHandlerErr = mkServantErr SS.err500

class HasHandlerConf m chanDB where
    handlerConf :: m (HandlerConf chanDB)

instance HasHandlerConf (AppM (HandlerConf chanDb)) chanDb where
    handlerConf = Reader.ask

class HasDbConf m chanDb where
    getDbConf :: m chanDb

instance HasDbConf (AppM (HandlerConf chanDb)) chanDb where
    getDbConf = Reader.asks hcChanDb

class HasHttpManager m where
    getManager :: m Manager
