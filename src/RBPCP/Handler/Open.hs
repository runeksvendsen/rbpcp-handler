module RBPCP.Handler.Open where

import RBPCP.Handler.Internal.Util
import Conf
import qualified Settings
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


runOpen :: DB.ChanDB m dbH
        => EitherT (HandlerErr OpenErr) m RBPCP.PaymentResult
        -> HandlerM dbH RBPCP.PaymentResult
runOpen payET = do
    cfg <- getDbConf
    let atomicEitherT = DB.runDB cfg $ runEitherT payET
    handleErrorE =<< handleErrorE =<< liftIO atomicEitherT

openE ::
     ( DB.ChanDB m conf
     , MonadIO m
     )
    => Maybe Wall.ConfirmationInfo
    -> RBPCP.BtcTxId
    -> Word32
    -> Maybe HC.Hash256
    -> RBPCP.Payment
    -> EitherT (HandlerErr OpenErr) m RBPCP.PaymentResult
openE _             _        _        Nothing       _                                  = left $ UserError ResourceNotFound
openE Nothing       fundTxId _        _             _                                  = abortWithErr $ TxNotFound fundTxId
openE (Just cInfo)  fundTxId fundIdx (Just secret) (RBPCP.Payment paymentData appData) = do
    maybeRedirect (fundTxId, fundIdx, secret) paymentData

    let confs = Wall.ciConfCount cInfo
    when (fromIntegral confs < Settings.confMinBtcConf) $
        abortWithErr $ InsufficientConfCount (fromIntegral confs) Settings.confMinBtcConf

    --    2. Verify payment/Create state object
    let tx = Wall.proof_tx_data $ Wall.ciFundProof cInfo
    (chanState, val) <- abortOnErr =<< fmapL OpeningPaymentError <$>
            liftIO (PC.channelFromInitialPayment tx paymentData)
    when (val < Settings.confOpenPrice) $
        abortWithErr $ InitialPaymentShort val Settings.confOpenPrice
    let stateSecret = PC.toHash $ PC.getSecret chanState
    when (secret /= stateSecret) $
        abortWithErr $ BadSharedSecret stateSecret

    --    3. PubKey-DB: Lookup
    let recvPubKey = PC.getRecvPubKey chanState
    pkIndexM <- lift $ DB.pubKeyLookup Settings.confServerExtPub recvPubKey
    chanStateX <- abortOnErr $ case pkIndexM of
        Nothing  -> Left  $ NoSuchServerPubKey $ PC.getPubKey recvPubKey
        Just pki -> Right $ PC.setMetadata chanState (DB.kaiIndex pki)

    --    4. ChanDB: mark pubKey as used & insert PayChanState
    _ <- lift $ DB.pubKeyMarkUsed Settings.confServerExtPub recvPubKey
    lift $ DB.create chanStateX

    return RBPCP.PaymentResult
           { paymentResult_channel_status     = RBPCP.ChannelOpen
           , paymentResult_channel_valueLeft  = fromIntegral $ PC.availableChannelVal chanState
           , paymentResult_value_received     = fromIntegral val
           , paymentResult_settlement_txid    = Nothing
           , paymentResult_application_data   = ""
           }


data OpenErr
  = TxNotFound RBPCP.BtcTxId
  | InsufficientConfCount Word Word
  | InitialPaymentShort PC.BtcAmount PC.BtcAmount
  | BadSharedSecret HC.Hash256
  | NoSuchServerPubKey HC.PubKeyC
  | OpeningPaymentError PC.PayChanError


getConfProof ::
       RBPCP.BtcTxId
    -> HandlerM dbH (Maybe Wall.ConfirmationInfo)
getConfProof tid = do
    --    1. get confirmation info for fundingTxId
    iface <- wallIface
    let txid = RBPCP.btcTxId tid
    handleErrorE =<< liftIO (runProofSC $ Wall.getConfProof iface txid)


-- Util
runProofSC :: MonadIO m => SC.ClientM a -> m (Either InternalError a)
runProofSC req = do
    man <- liftIO newTlsManager
    resE <- liftIO $ SC.runClientM req (clientEnv man)  -- TODO: error handling/Control.Retry
    return $ fmapL (RequestError Settings.confProofServer) resE
  where
    clientEnv m = SC.ClientEnv m Settings.confProofServer


instance IsHandlerException OpenErr where
    mkHandlerErr = mkServantErr SS.err400


instance Show OpenErr where
    show (TxNotFound tid) = unwords
        [ "resource tx"
        , show tid -- TODO <-
        , "not found in blockchain"
        ]
    show (InsufficientConfCount have need) = unwords
        [ "insufficient confirmation count. have"
        , show have
        , "need"
        , show need
        ]
    show (InitialPaymentShort found expectd) = unwords
        [ "initial payment of insufficient value. expected"
        , show expectd
        , "received"
        , show found
        ]
    show (BadSharedSecret ourSecret) = unwords
        [ "unexpected client/server secret. expected:", show ourSecret ]
    show (NoSuchServerPubKey pk) = unwords
        [ "unknown server pubkey", show pk ]
    show (OpeningPaymentError e) = unwords
        ["invalid opening payment:"
        , show e
        ]
