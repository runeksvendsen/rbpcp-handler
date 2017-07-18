module RBPCP.Handler.Close where

import MyPrelude
import RBPCP.Handler.Internal.Util
import Servant.API

import qualified RBPCP.Handler.Internal.BtcAddrIndex  as AddrIndex
import qualified Servant.Client                       as SC
import qualified RBPCP.Types                          as RBPCP
import qualified PaymentChannel                       as PC
import qualified ChanDB                               as DB
import qualified Network.Haskoin.Crypto               as HC
import qualified Network.Haskoin.Transaction          as HT
import qualified BitcoinSigner.Lib.Signing            as Sign
import qualified RBPCP.Handler.Internal.Blockchain    as Chain
-- DEBUG
import Data.ByteString.Base16                         as B16
import qualified Data.Serialize                 as Bin


runClose :: DB.ChanDBTx m dbM dbH
         => ReaderT (HandlerConf dbH chain) (EitherT (HandlerErr PC.PayChanError) m) RBPCP.PaymentResult
         -> HandlerM dbH chain RBPCP.PaymentResult
runClose closeRET = do
    conf <- getAppConf
    runAtomic $ runReaderT closeRET conf

closeE :: ( DB.ChanDBTx m dbM dbH
          , BlockchainRun m1 chain
          ) =>
         RBPCP.BtcTxId
      -> Word32
      -> Maybe RBPCP.SharedSecret
      -> RBPCP.Payment
      -> ReaderT (HandlerConf dbH chain) (EitherT (HandlerErr PC.PayChanError) m) RBPCP.PaymentResult
closeE _        _        Nothing       _                        = lift $ generalErr $ UserError ResourceNotFound
closeE fundTxId fundIdx (Just secret) (RBPCP.Payment payData _) = do
    lift $ maybeRedirect (fundTxId, fundIdx, secret) payData

    chanState <- maybe (lift $ generalErr $ UserError ResourceNotFound) return
                        =<< lift (lift $ DB.getPayChan secret)
    closedServerChan <- lift . abortOnErr =<< liftIO (PC.acceptClosingPayment payData chanState)

    -- Ask bitcoin-signer to sign settlement tx
    servConf <- asks hcServerConf
    let confBitcoinSigner = scBitcoinSigner servConf
    tx <- lift . hoistEither . fmapL InternalErr =<< internalReq confBitcoinSigner (settleClosed closedServerChan)

    -- Publish tx
--    liftIO . putStrLn . cs . B16.encode . Bin.encode $ tx
    lift . hoistEither . fmapL InternalErr =<< handlerChainReq (Chain.publishTx tx)

    -- lift . hoistEither . fmapL InternalErr =<< internalReq confProofServer (AddrIndex.publishTx tx)

    -- TODO: closedServerChan ready for next payment/settle tx?
    lift $ lift $ DB.updatePayChan (PC.getClosedState closedServerChan)

    return RBPCP.PaymentResult
             { paymentResultChannelStatus     = RBPCP.ChannelClosed
             , paymentResultChannelValueLeft  = 0
             , paymentResultValueReceived     = 0
             , paymentResultSettlementTxid    = Just . RBPCP.BtcTxId $ HT.txHash tx
             , paymentResultApplicationData   = ""
             }

settleClosed :: PC.ClosedServerChanX -> SC.ClientM HT.Tx
_ :<|> settleClosed :<|> _ :<|> _ = SC.client api
    where api :: Proxy Sign.BTCSign
          api  = Proxy























