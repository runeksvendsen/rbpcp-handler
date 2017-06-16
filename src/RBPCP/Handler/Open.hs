module RBPCP.Handler.Open where

import RBPCP.Handler.Internal.Util
import RBPCP.Handler.Internal.Funding

import qualified Settings
import qualified Servant.Server               as SS
import qualified RBPCP.Types                  as RBPCP
import qualified PaymentChannel               as PC
import qualified ChanDB                       as DB
import qualified Bitcoin.SPV.Wallet           as Wall
import qualified Network.Haskoin.Crypto       as HC


runOpen :: DB.ChanDB m dbH
        => ReaderT (HandlerConf dbH) (EitherT (HandlerErr OpenErr) m) RBPCP.PaymentResult
        -> HandlerM dbH RBPCP.PaymentResult
runOpen openET = do
    cfg <- getAppConf
    runNonAtomic $ runReaderT openET cfg

openE ::
     ( DB.ChanDB m conf )
    => RBPCP.BtcTxId
    -> Word32
    -> Maybe HC.Hash256
    -> RBPCP.Payment
    -> ReaderT (HandlerConf conf) (EitherT (HandlerErr OpenErr) m) RBPCP.PaymentResult
openE _        _        Nothing       _                                  = lift $ left $ UserError ResourceNotFound
openE fundTxId fundIdx (Just secret) (RBPCP.Payment paymentData _) = do
    lift $ maybeRedirect (fundTxId, fundIdx, secret) paymentData

    --    1. Fetch alleged funding Bitcoin transaction
    let txid = RBPCP.btcTxId fundTxId
    fundProof <- maybe (lift $ abortWithErr $ TxNotFound fundTxId) return
              =<< lift . hoistEither . fmapL InternalErr
              =<< internalReq Settings.confProofServer (fundingProofM txid)

    --    2. Verify payment/Create state object
    let tx = Wall.proof_tx_data fundProof
    chanState <- lift . abortOnErr =<< fmapL OpeningPaymentError <$>
            liftIO (PC.channelFromInitialPayment Settings.serverSettings tx paymentData)
    let stateSecret = PC.toHash $ PC.getSecret chanState
    when (secret /= stateSecret) $
        lift $ abortWithErr $ BadSharedSecret stateSecret

    --    3. Check if funding output is spent, plus confirmation count
    let fundAddr = PC.fundingAddress chanState
        -- Important: 'maybeRedirect' checks if 'fundTxId'/'fundIdx' and hash/idx in 'paymentData' match
        relevantAddrInfo afi = asiFundingTxId afi == txid && asiFundingVout afi == fundIdx
    addrFundInfoL <- lift . hoistEither . fmapL InternalErr
                  =<< internalReq Settings.confProofServer (unspentOuts fundAddr)
    case filter relevantAddrInfo addrFundInfoL of
        []    -> lift $ abortWithErr SpentFundingOutput
        [afi] -> do
            let confs = asiConfs afi
            when (fromIntegral confs < Settings.confMinBtcConf) $
                lift $ abortWithErr $ InsufficientConfCount (fromIntegral confs) Settings.confMinBtcConf
        x@(_:_:_) -> error $ "BUG: Multiple outputs with same txid+vout: " ++ show x


    --    4. PubKey-DB: Lookup
    let recvPubKey = PC.getRecvPubKey chanState
    pkIndexM <- lift $ lift $ DB.pubKeyLookup Settings.confServerExtPub recvPubKey
    chanStateX <- lift $ abortOnErr $ case pkIndexM of
        Nothing  -> Left  $ NoSuchServerPubKey $ PC.getPubKey recvPubKey
        Just pki -> Right $ PC.setMetadata chanState (DB.kaiIndex pki)

    --    5. ChanDB: mark pubKey as used & insert PayChanState
    _ <- lift $ lift $ DB.pubKeyMarkUsed Settings.confServerExtPub recvPubKey
    lift $ lift $ DB.create chanStateX

    return RBPCP.PaymentResult
           { paymentResult_channel_status     = RBPCP.ChannelOpen
           , paymentResult_channel_valueLeft  = fromIntegral $ PC.channelValueLeft chanState
           , paymentResult_value_received     = fromIntegral $ PC.serverConfOpenPrice Settings.serverSettings
           , paymentResult_settlement_txid    = Nothing
           , paymentResult_application_data   = ""
           }


data OpenErr
  = TxNotFound RBPCP.BtcTxId
  | SpentFundingOutput
  | InsufficientConfCount Word Word
  | InitialPaymentShort PC.BtcAmount PC.BtcAmount
  | BadSharedSecret HC.Hash256
  | NoSuchServerPubKey HC.PubKeyC
  | OpeningPaymentError PC.PayChanError

instance IsHandlerException OpenErr where
    mkHandlerErr = mkServantErr SS.err400

instance Show OpenErr where
    show (TxNotFound tid) = unwords
        [ "resource tx"
        , show tid -- TODO <-
        , "not found in blockchain"
        ]
    show SpentFundingOutput = "channel funding output already spent"
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
