{-# LANGUAGE TemplateHaskell #-}
module RBPCP.Handler.Internal.Util
( module RBPCP.Handler.Internal.Util
, module RBPCP.Handler.Internal.Types
, module MyPrelude
)
where

import RBPCP.Handler.Internal.Types
import MyPrelude

import qualified Conf
import qualified ChanDB                       as DB
import qualified PaymentChannel               as PC
import qualified RBPCP.Types                  as RBPCP
import qualified RBPCP.Api                    as API
import qualified Web.HttpApiData              as Web
import qualified Servant.Utils.Links          as SL
import qualified Bitcoin.SPV.Wallet           as Wall
import qualified Data.ByteString.Base16       as B16
import qualified Control.Monad.Reader         as Reader
import qualified Network.Haskoin.Crypto       as HC
import qualified Control.Monad.Logger         as Log
import qualified Servant.Server               as SS
import qualified Servant.Client               as SC
import           Network.Bitcoin.AddrIndex.API  (PublishTx, PushTxReq(..))


runAtomic :: DB.ChanDBTx m dbM dbH
          => EitherT (HandlerErr PC.PayChanError) m a
          -> HandlerM dbH a
runAtomic atomicET = do
    cfg <- getDbConf
    let atomic = DB.atomically DB.PayChanDB cfg $ runEitherT atomicET
    handleErrorE =<< handleErrorE =<< liftIO atomic

runNonAtomic ::
    ( IsHandlerException e
    , DB.ChanDB m dbH
    )
    => EitherT (HandlerErr e) m a
    -> HandlerM dbH a
runNonAtomic atomicET = do
    cfg <- getDbConf
    let nonAtomic = DB.runDB cfg $ runEitherT atomicET
    handleErrorE =<< handleErrorE =<< liftIO nonAtomic

maybeRedirect :: Monad m
    => (RBPCP.BtcTxId, Word32, HC.Hash256)
    -> RBPCP.PaymentData
    -> EitherT (HandlerErr e) m ()
maybeRedirect (txid,vout,s) RBPCP.PaymentData{..} =
    when (paymentDataFundingTxid /= txid
       || paymentDataFundingVout /= vout) redirect
            where redirect = left $ UserError $ ResourcePaymentMismatch txid vout s

abortOnErr :: Monad m => Either e a -> EitherT (HandlerErr e) m a
abortOnErr = hoistEither . fmapL HandlerErr

abortWithErr :: Monad m => e -> EitherT (HandlerErr e) m a
abortWithErr = left . HandlerErr

generalErr :: Monad m => HandlerErr e -> EitherT (HandlerErr e) m a
generalErr = left

liftDb :: (Monad m, Monad (t m), Reader.MonadTrans t) => m a -> (t (t m)) a
liftDb = lift . lift

{-
class MayReturnError e m where
    abortWithErr  :: e -> m a
    abortOnErr    :: Either e a -> m a
    generalErr    :: HandlerErr e -> m a

instance Monad m => MayReturnError e (EitherT (HandlerErr e) m) where
    abortOnErr = hoistEither . fmapL HandlerErr
    abortWithErr = left . HandlerErr
    generalErr = left

instance Monad m => MayReturnError e (ReaderT a (EitherT (HandlerErr e) m)) where
    abortOnErr = lift . abortOnErr
    abortWithErr = lift . abortWithErr
    generalErr = lift . generalErr
-}

handleErrorE ::
    ( MonadError SS.ServantErr m
    , MonadIO m
    , IsHandlerException e
    ) => Either e a -> m a
handleErrorE =
    either handleError return

handleError ::
    ( MonadError SS.ServantErr m
    , MonadIO m
    , IsHandlerException e
    ) => e -> m a
handleError =
    throwError <=< logInternalErr . mkHandlerErr
  where
    logInternalErr e = when (SS.errHTTPCode e == 500)
          (Log.runStdoutLoggingT ($(Log.logErrorSH) e))
          >> return e

handlerRunDb :: DB.ChanDB m dbH => m a -> HandlerM dbH a
handlerRunDb m =
    getDbConf >>= liftIO . (`DB.runDB` m) >>= handleErrorE


publishTx :: BitcoinTx -> SC.ClientM ()
publishTx = void . SC.client api . PushTxReq
    where api :: Proxy PublishTx
          api  = Proxy
