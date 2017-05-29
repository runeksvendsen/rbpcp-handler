module RBPCP.Handler.Pay where

import RBPCP.Handler.Internal.Util
import Settings
import Conf
import Servant.Server
import Control.Monad.Time
import Control.Monad.Catch

import qualified Servant.Client               as SC
import qualified Servant.Server               as SS
import qualified RBPCP.Types                  as RBPCP
import qualified RBPCP.Api                    as API
import qualified PaymentChannel               as PC
import qualified ChanDB                       as DB
import qualified Bitcoin.SPV.Wallet           as Wall
import qualified Bitcoin.SPV.Wallet.Extra     as Wall
import qualified Network.Haskoin.Crypto       as HC
import qualified Data.Text                    as T


data PaymentError
  = PaymentError PC.PayChanError
  | ApplicationError T.Text

instance IsHandlerException PaymentError where
    mkHandlerErr (PaymentError pce) = mkServantErr SS.err400 pce
    -- TODO: separate payment error from application error
    mkHandlerErr (ApplicationError e) = mkServantErr SS.err400 e

newtype PaymentCallback = PaymentCallback
    (  T.Text               -- ^ Application data
    -> PC.ServerPayChanX    -- ^ New server state
    -> PC.BtcAmount         -- ^ Payment value
    -> IO CallbackResult    -- ^ Either error or application response data
    )

-- | Either error or application response data
newtype CallbackResult = CallbackResult (Either T.Text T.Text)


runPay :: DB.ChanDBTx m dbM dbH
    => PaymentCallback
    -> ReaderT PaymentCallback (EitherT (HandlerErr PaymentError) m) RBPCP.PaymentResult
    -> HandlerM dbH RBPCP.PaymentResult
runPay cbFunc payM =
    runAtomic $ runReaderT payM cbFunc

payE :: ( DB.ChanDBTx m dbM dbH
        ) =>
        RBPCP.BtcTxId
     -> Word32
     -> Maybe HC.Hash256
     -> RBPCP.Payment
     -> ReaderT PaymentCallback (EitherT (HandlerErr PaymentError) m) RBPCP.PaymentResult
payE _        _        Nothing       _                              = lift $ left $ UserError ResourceNotFound
payE fundTxId fundIdx (Just secret) (RBPCP.Payment payData appData) = do
    lift $ maybeRedirect (fundTxId, fundIdx, secret) payData

    let throwNotFound = maybe (lift $ left $ UserError ResourceNotFound) return
    (newState, payVal) <- lift . abortOnErr . fmapL PaymentError
                            =<< liftIO . PC.acceptPayment payData
                            =<< throwNotFound
                            =<< lift (lift $ DB.getPayChan $ PC.fromHash secret)

    PaymentCallback callbackFunc <- ask
    CallbackResult  callbackResE <- liftIO $ callbackFunc appData newState payVal
    appResData   <- case callbackResE of
                        Left e  -> lift $ abortWithErr $ ApplicationError e
                        Right r -> return r

    -- Save state to DB
    lift $ lift $ DB.updatePayChan newState

    return RBPCP.PaymentResult
           { paymentResult_channel_status     =
                  if PC.channelValueLeft newState /= 0
                      then RBPCP.ChannelOpen
                      else RBPCP.ChannelClosed
           , paymentResult_channel_valueLeft  = fromIntegral $ PC.channelValueLeft newState
           , paymentResult_value_received     = fromIntegral payVal
           , paymentResult_settlement_txid    = Nothing
           , paymentResult_application_data   = appResData
           }


