{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RBPCP.Handler.Open where

import RBPCP.Handler.Internal.Util
import RBPCP.Handler.Internal.Funding

import qualified Servant.Server               as SS
import qualified RBPCP.Types                  as RBPCP
import qualified PaymentChannel               as PC
import qualified ChanDB                       as DB
import qualified Network.Haskoin.Crypto       as HC
import qualified Control.Monad.Logger         as Log


runOpen :: DB.ChanDB m dbH
        => ReaderT (HandlerConf dbH) (EitherT (HandlerErr OpenErr) m) RBPCP.PaymentResult
        -> HandlerM dbH RBPCP.PaymentResult
runOpen openET = do
    cfg <- getAppConf
    runNonAtomic $ runReaderT openET cfg


--newtype OpenM m conf a = OpenM
--    { runOpenM :: ReaderT (HandlerConf conf) (EitherT (HandlerErr OpenErr) m) a
--    } deriving
--    ( Functor
--    , Applicative
--    , Log.MonadLogger
--    , Monad
--    , R.MonadReader (HandlerConf conf)
--    , MonadResource
--    )

openE ::
     ( DB.ChanDB m conf )
    => RBPCP.BtcTxId
    -> Word32
    -> Maybe RBPCP.SharedSecret
    -> RBPCP.Payment
    -> ReaderT (HandlerConf conf) (EitherT (HandlerErr OpenErr) m) RBPCP.PaymentResult
openE _        _        Nothing       _                                  = lift $ left $ UserError ResourceNotFound
openE fundTxId fundIdx (Just secret) (RBPCP.Payment paymentData _) = do
    lift $ maybeRedirect (fundTxId, fundIdx, secret) paymentData

    ServerConf{..} <- asks hcServerConf

    --    1. Fetch alleged funding Bitcoin transaction
    let txid = RBPCP.btcTxId fundTxId
    tx <- maybe (lift $ abortWithErr $ TxNotFound fundTxId) return
              =<< lift . hoistEither . fmapL InternalErr
              =<< internalReq scProofServer (fetchBtcTxM txid)

    --    2. Verify payment/Create state object
    chanState <- lift . abortOnErr =<< fmapL OpeningPaymentError <$>
            liftIO (PC.channelFromInitialPayment scSettings tx paymentData)
    let stateSecret = PC.getSecret chanState
    when (secret /= stateSecret) $
        lift $ abortWithErr $ BadSharedSecret stateSecret

    --    3. Check if funding output is spent, plus confirmation count
    let fundAddr = PC.fundingAddress chanState
        -- Important: 'maybeRedirect' checks if 'fundTxId'/'fundIdx' and hash/idx in 'paymentData' match
        relevantAddrInfo afi = asiFundingTxId afi == txid && asiFundingVout afi == fundIdx
    addrFundInfoL <- lift . hoistEither . fmapL InternalErr
                  =<< internalReq scProofServer (unspentOuts fundAddr)
    case filter relevantAddrInfo addrFundInfoL of
        []        -> lift $ abortWithErr SpentFundingOutput
        x@(_:_:_) -> error $ "BUG: Multiple outputs with same txid+vout: " ++ show x
        [afi]     -> do
            let confs = asiConfs afi
            when (fromIntegral confs < scMinBtcConf) $
                lift $ abortWithErr $ InsufficientConfCount (fromIntegral confs) scMinBtcConf

    --    4. Check server pubkey
    rootPub <- asks hcPubKey
    chanStateX <- lift $ abortOnErr $ case PC.mkExtendedDerivRpc rootPub chanState of
        Just state -> Right state
        Nothing    -> Left . UnknownServerPubKey . PC.getPubKey $ PC.getRecvPubKey chanState

    --    5. ChanDB: Insert PayChanState
    lift $ lift $ DB.create chanStateX
    liftIO $ putStrLn $ unwords
        [ "Opened channel with server pubkey: ", show (PC.getPubKey $ PC.getRecvPubKey chanStateX) ]

    return RBPCP.PaymentResult
           { paymentResultChannelStatus     = RBPCP.ChannelOpen
           , paymentResultChannelValueLeft  = fromIntegral $ PC.channelValueLeft chanState
           , paymentResultValueReceived     = fromIntegral $ PC.serverConfOpenPrice scSettings
           , paymentResultSettlementTxid    = Nothing
           , paymentResultApplicationData   = ""
           }


data OpenErr
  = TxNotFound RBPCP.BtcTxId
  | SpentFundingOutput
  | InsufficientConfCount Word Word
  | InitialPaymentShort PC.BtcAmount PC.BtcAmount
  | BadSharedSecret RBPCP.SharedSecret
  | UnknownServerPubKey HC.PubKeyC
  | OpeningPaymentError PC.PayChanError

instance IsHandlerException OpenErr where
    mkHandlerErr = mkServantErr SS.err400

instance HasErrorResponse OpenErr where
    errRes = cs . show

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
    show (UnknownServerPubKey pk) = unwords
        [ "unknown server pubkey", show pk ]
    show (OpeningPaymentError e) = unwords
        ["invalid opening payment:"
        , show e
        ]
