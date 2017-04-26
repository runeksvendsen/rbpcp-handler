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



runAtomic :: DB.ChanDBTx m dbM dbH
          => EitherT (HandlerErr PC.PayChanError) m a
          -> HandlerM dbH a
runAtomic atomicET = do
    cfg <- getDbConf
    let atomicEitherT = DB.atomically DB.PayChanDB cfg $ runEitherT atomicET
    handleErrorE =<< handleErrorE =<< liftIO atomicEitherT

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


