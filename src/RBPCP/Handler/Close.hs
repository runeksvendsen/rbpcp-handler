module RBPCP.Handler.Close where

--import           AppPrelude.Types
--import           PayChanServer.Types
--import           PayChanServer.Util
--import qualified PayChanServer.Config.Types as Conf
import qualified RBPCP.Types as RBPCP

--import qualified PayChanServer.DB as DB
--import           ChanStore.Interface  as DBConn
--import           AppPrelude.Man


chanSettleHandler ::
    PayChanConf m
    => SendPubKey
    -> LockTimeDate
    -> TxHash
    -> Vout
    -> RBPCP.Payment
    -> m PaymentResult
chanSettleHandler sendPK lockTime fundTxId fundIdx (RBPCP.Payment payment appData) = do
    dbConn <- confGet Conf.dbInterface
    settleChannel <- confGet Conf.settleChannel
    -- Ask ChanStore to begin closing channel, if everything matches up
    let closeReq = CloseBeginRequest
            (ChannelResource sendPK lockTime (OutPoint fundTxId fundIdx)) payment
    closeRes <- liftIO $ DBConn.settleByInfoBegin dbConn closeReq
    -- Decide what to do based on channel state: open/half-open/closed
    (settlementTxId,chanValLeft) <- case closeRes of
            CloseInitiated (rpc,origVal)        -> do
                settleTxId <- liftIO (settleChannel rpc)
                return (settleTxId, channelValueLeft rpc)
            ClosingPaymentError e                   ->
                userError' $ show e
            CloseUpdateError (UpdChanClosed settleTxId chanValLeft) ->
                return (settleTxId, chanValLeft)
            CloseUpdateError ChanBeingClosed    ->
                errorWithDescription 410 "Channel is being closed"
            CloseUpdateError NoSuchChannel      ->
                errorWithDescription 404 "No such channel"
    -- Write response
    return PaymentResult
           { paymentResult_channel_status     = ChannelClosed
           , paymentResult_channel_valueLeft  = chanValLeft
           , paymentResult_value_received     = 0
           , paymentResult_settlement_txid    = Just settlementTxId
           , paymentResult_application_data   = ""
           }

