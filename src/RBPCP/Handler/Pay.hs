module RBPCP.Handler.Pay where

import RBPCP.Handler.Internal.Util

import qualified Servant.Server               as SS
import qualified RBPCP.Types                  as RBPCP
import qualified PaymentChannel               as PC
import qualified ChanDB                       as DB
import qualified Network.Haskoin.Crypto       as HC
import qualified Data.Text                    as T


data PaymentError
  = PaymentError PC.PayChanError
  | ApplicationError T.Text

instance Show PaymentError where
    show (PaymentError e) =
        "payment error: " ++ show e
    show (ApplicationError t) =
        "application error: " ++ show t

instance IsHandlerException PaymentError where
    mkHandlerErr = mkServantErr SS.err400

instance HasErrorResponse PaymentError where
    errRes = cs . show

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
    -> HandlerM dbH chain RBPCP.PaymentResult
runPay cbFunc payM =
    runAtomic $ runReaderT payM cbFunc

payE :: ( DB.ChanDBTx m dbM dbH
        ) =>
        RBPCP.BtcTxId
     -> Word32
     -> Maybe RBPCP.SharedSecret
     -> RBPCP.Payment
     -> ReaderT PaymentCallback (EitherT (HandlerErr PaymentError) m) RBPCP.PaymentResult
payE _        _        Nothing       _                              = lift $ left $ UserError ResourceNotFound
payE fundTxId fundIdx (Just secret) (RBPCP.Payment payData appData) = do
    lift $ maybeRedirect (fundTxId, fundIdx, secret) payData

    let throwNotFound = maybe (lift $ left $ UserError ResourceNotFound) return
    (newState, payVal) <- lift . abortOnErr . fmapL PaymentError
                            =<< liftIO . PC.acceptPayment payData
                            =<< throwNotFound
                            =<< lift (lift $ DB.getPayChan secret)

    PaymentCallback callbackFunc <- ask
    CallbackResult  callbackResE <- liftIO $ callbackFunc appData newState payVal
    appResData   <- case callbackResE of
                        Left e  -> lift $ abortWithErr $ ApplicationError e
                        Right r -> return r

    -- Save state to DB
    lift $ lift $ DB.updatePayChan newState

    return RBPCP.PaymentResult
           { paymentResultChannelStatus     =
                  if PC.channelValueLeft newState /= 0
                      then RBPCP.ChannelOpen
                      else RBPCP.ChannelClosed
           , paymentResultChannelValueLeft  = fromIntegral $ PC.channelValueLeft newState
           , paymentResultValueReceived     = fromIntegral payVal
           , paymentResultSettlementTxid    = Nothing
           , paymentResultApplicationData   = appResData
           }


