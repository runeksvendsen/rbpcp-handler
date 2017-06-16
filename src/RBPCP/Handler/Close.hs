module RBPCP.Handler.Close where

import RBPCP.Handler.Internal.Util
import Settings
import Servant.API

import qualified Servant.Client               as SC
import qualified RBPCP.Types                  as RBPCP
import qualified PaymentChannel               as PC
import qualified ChanDB                       as DB
import qualified Bitcoin.SPV.Wallet           as Wall
import qualified Network.Haskoin.Crypto       as HC
import qualified Network.Haskoin.Transaction  as HT
import qualified BitcoinSigner.Lib.Signing    as Sign


runClose :: DB.ChanDBTx m dbM dbH
         => ReaderT (HandlerConf dbH) (EitherT (HandlerErr PC.PayChanError) m) RBPCP.PaymentResult
         -> HandlerM dbH RBPCP.PaymentResult
runClose closeRET = do
    conf <- getAppConf
    runAtomic $ runReaderT closeRET conf

closeE :: ( DB.ChanDBTx m dbM dbH
          -- , MonadIO m
          ) =>
         RBPCP.BtcTxId
      -> Word32
      -> Maybe HC.Hash256
      -> RBPCP.Payment
      -> ReaderT (HandlerConf dbH) (EitherT (HandlerErr PC.PayChanError) m) RBPCP.PaymentResult
closeE _        _        Nothing       _                        = lift $ generalErr $ UserError ResourceNotFound
closeE fundTxId fundIdx (Just secret) (RBPCP.Payment payData _) = do
    lift $ maybeRedirect (fundTxId, fundIdx, secret) payData

    chanState <- maybe (lift $ generalErr $ UserError ResourceNotFound) return
                        =<< lift (lift $ DB.getPayChan $ PC.fromHash secret)
    closedServerChan <- lift . abortOnErr =<< liftIO (PC.acceptClosingPayment payData chanState)

    -- Ask bitcoin-signer to sign settlement tx
    tx <- lift . hoistEither . fmapL InternalErr =<< internalReq confBitcoinSigner (settleClosed closedServerChan)

    -- Publish tx
    lift . hoistEither . fmapL InternalErr =<< internalReq confProofServer (publishTx tx)

    -- TODO: closedServerChan ready for next payment/settle tx?
    lift $ lift $ DB.updatePayChan (PC.getClosedState closedServerChan)

    return RBPCP.PaymentResult
             { paymentResult_channel_status     = RBPCP.ChannelClosed
             , paymentResult_channel_valueLeft  = 0
             , paymentResult_value_received     = 0
             , paymentResult_settlement_txid    = Just . RBPCP.BtcTxId $ HT.txHash tx
             , paymentResult_application_data   = ""
             }


settleClosed :: PC.ClosedServerChanX -> SC.ClientM HT.Tx
settleClosed :<|> _ :<|> _ = SC.client api
    where api :: Proxy Sign.BTCSign
          api  = Proxy























