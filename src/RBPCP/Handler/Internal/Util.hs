{-# LANGUAGE TemplateHaskell #-}
module RBPCP.Handler.Internal.Util
( module RBPCP.Handler.Internal.Util
, module RBPCP.Handler.Internal.Types
, module MyPrelude
)
where

import RBPCP.Handler.Internal.Types
import MyPrelude
import Network.Bitcoin.AddrIndex.API          (PublishTx, PushTxReq(..))
import qualified ChanDB                       as DB
import qualified PaymentChannel               as PC
import qualified RBPCP.Types                  as RBPCP
import qualified Control.Monad.Reader         as Reader
import qualified Network.Haskoin.Transaction  as HT
import qualified Network.Haskoin.Crypto       as HC
import qualified Control.Monad.Logger         as Log
import qualified Servant.Server               as SS
import qualified Servant.Client               as SC


runAtomic ::
    ( DB.ChanDBTx m dbM dbH
    , IsHandlerException e
    )
    => EitherT (HandlerErr e) m a
    -> HandlerM dbH a
runAtomic atomicET = do
    cfg <- getDbConf
    let atomic = DB.atomically DB.PayChanDB cfg $ runEitherT atomicET
    handleErrorE =<< handleErrorE =<< liftIO atomic

runNonAtomic ::
    ( DB.ChanDB m dbH
    , IsHandlerException e
    )
    => EitherT (HandlerErr e) m a
    -> HandlerM dbH a
runNonAtomic nonAtomicET = do
    cfg <- getDbConf
    let nonAtomic = DB.runDB cfg $ runEitherT nonAtomicET
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
