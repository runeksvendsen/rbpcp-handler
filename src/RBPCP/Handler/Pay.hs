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
import qualified Network.Haskoin.Transaction  as HT


payE :: ( DB.ChanDBTx m dbM dbH
        -- , MonadIO m
        ) =>
        RBPCP.BtcTxId
     -> Word32
     -> Maybe HC.Hash256
     -> RBPCP.Payment
     -> EitherT (HandlerErr PC.PayChanError) m RBPCP.PaymentResult
payE _        _        Nothing       _                              = left $ UserError ResourceNotFound
payE fundTxId fundIdx (Just secret) (RBPCP.Payment payData appData) = do
    maybeRedirect (fundTxId, fundIdx, secret) payData

    let throwNotFound = maybe (left $ UserError ResourceNotFound) return
    (newState, payVal) <- abortOnErr  =<< liftIO . PC.acceptPayment payData
                                      =<< throwNotFound
                                      =<< lift (DB.getPayChan $ PC.fromHash secret)

    -- TODO: "callback payVal appData"

    -- Save state to DB
    lift $ DB.updatePayChan newState

    return RBPCP.PaymentResult
           { paymentResult_channel_status     =
                  if PC.availableChannelVal newState /= 0
                      then RBPCP.ChannelOpen
                      else RBPCP.ChannelClosed
           , paymentResult_channel_valueLeft  = fromIntegral $ PC.availableChannelVal newState
           , paymentResult_value_received     = fromIntegral payVal
           , paymentResult_settlement_txid    = Nothing
           , paymentResult_application_data   = ""
           }




{-
data CallbackInfo = CallbackInfo
  { amount              :: BtcAmount
  , chan_value_left     :: BtcAmount
  , chan_total_value    :: BtcAmount
  , client_app_data     :: T.Text
  , full_payment        :: SignedPayment
  } deriving (Generic, FromJSON, ToJSON)

data CallbackResponse = CallbackResponse
  { resp_app_data       :: T.Text
  , resp_app_error      :: Maybe T.Text
  }
-}








