module RBPCP.Handler.Close where

import RBPCP.Handler.Internal.Util
import Settings
import Servant.Server
import Control.Monad.Time
import Control.Monad.Catch
import Servant.API

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
import qualified BitcoinSigner.Lib.Signing    as Sign


runClose :: DB.ChanDBTx m dbM dbH
         => EitherT (HandlerErr PC.PayChanError) m (RBPCP.PaymentResult,HT.Tx)
         -> HandlerM dbH RBPCP.PaymentResult
runClose closeET = do
    (res,tx) <- runAtomic closeET
    -- Publish tx
    iface <- wallIface
    _ <- liftIO $ Wall.importTx iface tx
    -- TODO: Also push using blockchain-restful-address-index?
    return res

closeE :: ( DB.ChanDBTx m dbM dbH
          , MonadIO m
          ) =>
         RBPCP.BtcTxId
      -> Word32
      -> Maybe HC.Hash256
      -> RBPCP.Payment
      -> EitherT (HandlerErr PC.PayChanError) m (RBPCP.PaymentResult,HT.Tx)
closeE _        _        Nothing       _                        = left $ UserError ResourceNotFound
closeE fundTxId fundIdx (Just secret) (RBPCP.Payment payData _) = do
    -- maybeRedirect (fundTxId, fundIdx, secret) payData

    let throwNotFound = maybe (left $ UserError ResourceNotFound) return
    chanState <- throwNotFound =<< lift (DB.getPayChan $ PC.fromHash secret)
    -- Tx fee equals value of closing payment
    closedServerChan <- abortOnErr =<< liftIO (PC.acceptClosingPayment payData chanState)

    -- Ask bitcoin-signer to sign settlement tx
    tx <- runBtcSignSC $ settleClosed closedServerChan

    -- TODO: closedServerChan ready for next payment/settle tx?
    lift $ DB.updatePayChan (PC.getClosedState closedServerChan)

    return ( RBPCP.PaymentResult
             { paymentResult_channel_status     = RBPCP.ChannelClosed
             , paymentResult_channel_valueLeft  = 0
             , paymentResult_value_received     = 0
             , paymentResult_settlement_txid    = Just . RBPCP.BtcTxId $ HT.txHash tx
             , paymentResult_application_data   = ""
             }
           , tx
           )

settleClosed :: PC.ClosedServerChanX -> SC.ClientM HT.Tx
settleClosed :<|> _ :<|> _ = SC.client api
    where api :: Proxy Sign.BTCSign
          api  = Proxy

-- Util
runBtcSignSC :: MonadIO m => SC.ClientM a -> EitherT (HandlerErr e) m a
runBtcSignSC = runClientM confBitcoinSigner

runClientM :: MonadIO m => SC.BaseUrl -> SC.ClientM a -> EitherT (HandlerErr e) m a
runClientM url req = do
    man <- liftIO newTlsManager
    resE <- liftIO $ SC.runClientM req (clientEnv man)  -- TODO: error handling/Control.Retry
    hoistEither $ fmapL (InternalErr . RequestError url) resE
  where
    clientEnv m = SC.ClientEnv m url
