{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RBPCP.Handler.Internal.Blockchain
( -- * Class
  MonadBlockchain(..)
, BlockchainRun(..)
  -- * Implementations
, BitcoinProdnet
, BitcoinTestnet
, TestDerive

, BlockchainConf(..)
, AddrFunding
-- * Re-exports
, Net.BtcNetwork(..)
, Net.Prodnet(..)
, Net.Testnet3(..)
, Net.DisabledNet(..)
, AddressFundingInfo(..)
)
where

import MyPrelude
--import qualified RBPCP.Handler.Conf           as Cfg
import qualified RBPCP.Handler.Internal.Funding.Test  as Test
import qualified RBPCP.Handler.Internal.Funding.Network as Net
import Servant.Client
import RBPCP.Handler.Internal.BtcAddrIndex
import qualified PaymentChannel               as PC
import qualified RBPCP.Handler.Internal.BtcAddrIndex  as AddrIndex
import qualified Control.Monad.Reader                 as R
import qualified Control.Monad.Logger                 as Log
import qualified Network.Bitcoin.AddrIndex.Types      as AI
import qualified Network.Haskoin.Transaction    as HT
import qualified Control.Monad.Catch              as Catch
import Control.Monad.Error.Class


class Monad m => MonadBlockchain m net | m -> net where
    getChannelFunding
        :: PC.ChanParams                          -- ^ Address for which to do 'AddrFunding' lookup
        -> ([AddrFunding] -> Maybe AddrFunding)   -- ^ Which 'AddrFunding' to select out of the available
        -> m (Maybe (AddrFunding, HT.Tx))         -- ^ Return selected 'AddrFunding' plus the tx it points to
    publishTx
        :: HT.Tx
        -> m ()

class (Net.BtcNetwork net, MonadBlockchain m net) => BlockchainRun m net | net -> m where
    runBlockchain
        :: ReqMan
        -> BlockchainConf net   -- ^ Config
        -> m a                  -- ^ 'MonadBlockchain' action
        -> IO (Either InternalError a)

-- | Short-hand
type AddrFunding = AddressFundingInfo

data BlockchainConf net
  = BlockchainConf
  { bcServerUrl  :: AI.AddrIndexServerUrl
  , bcLogLevel   :: Log.LogLevel
  , bcChainNet   :: net
  }

instance MonadBlockchain (Bitcoin btcNet) btcNet where
    publishTx = bitcoinClientM . AddrIndex.publishTx
    getChannelFunding = getBitcoinFunding

instance BlockchainRun BitcoinProdnet Net.Prodnet where
    runBlockchain man cfg =
        runBitcoin (BitcoinConf (bcLogLevel cfg) man url)
      where
        url = AI.aisuLivenetServer . bcServerUrl $ cfg

instance BlockchainRun BitcoinTestnet Net.Testnet3 where
    runBlockchain man cfg =
        runBitcoin (BitcoinConf (bcLogLevel cfg) man url)
      where
        url = AI.aisuTestnetServer . bcServerUrl $ cfg


instance BlockchainRun TestDerive Net.DisabledNet where
    runBlockchain _ _ (TestDerive ioa) = Catch.try ioa

instance MonadBlockchain TestDerive Net.DisabledNet where
    getChannelFunding cp _ = deriveTestFunding cp
    publishTx = const $ return ()


type BitcoinProdnet = Bitcoin Net.Prodnet
type BitcoinTestnet = Bitcoin Net.Testnet3

newtype Bitcoin btcNet a = BitcoinProdnet
    { runBtcNet :: Log.LoggingT (R.ReaderT BitcoinConf (EitherT InternalError IO)) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , R.MonadReader BitcoinConf
        , Log.MonadLogger
        , R.MonadIO
        , MonadError InternalError
        )

data BitcoinConf =
    BitcoinConf
  { btcLogLevel   :: Log.LogLevel
  , btcReqMan     :: ReqMan
  , btcServerUrl  :: BaseUrl
  }

runBitcoin :: MonadIO m => BitcoinConf -> Bitcoin btcNet a -> m (Either InternalError a)
runBitcoin cfg mb =
    R.liftIO $ runEitherT $ R.runReaderT (Log.runStdoutLoggingT addrIndexLog) cfg
  where
    addrIndexLog = applyLogFilter (btcLogLevel cfg) (runBtcNet mb)


-- | Throws 'InternalError' on request failure
getBitcoinFunding
    :: PC.ChanParams
    -> ([AddrFunding] -> Maybe AddrFunding)
    -> Bitcoin btcNet (Maybe (AddrFunding, HT.Tx))
getBitcoinFunding cp getAf =
    bitcoinClientM (bitcoinFundingCM cp getAf)

-- | Throws 'InternalError' on request failure
bitcoinClientM
    :: ClientM a
    -> Bitcoin btcNet a
bitcoinClientM clientM = do
    man <- asks btcReqMan
    url <- asks btcServerUrl
    either throwError return . fmapL (RequestError url)
        =<< liftIO (runServantClient man url clientM)

bitcoinFundingCM
    :: PC.ChanParams
    -> ([AddrFunding] -> Maybe AddrFunding)
    -> ClientM (Maybe (AddrFunding, HT.Tx))
bitcoinFundingCM cp getAf = do
    addrFundL <- unspentFundingOuts cp
    let maybeAddrFund = getAf addrFundL
    case maybeAddrFund of
        Nothing -> return Nothing
        Just af -> do
            txM <- fundingTx $ asiFundingTxId af
            let tx = fromMaybe (error $ "getBitcoinFunding: tx not found for: " ++ show af) txM
            return $ Just (af,tx)

unspentFundingOuts :: PC.ChanParams -> ClientM [AddressFundingInfo]
unspentFundingOuts = AddrIndex.unspentOuts . PC.getFundingAddress

fundingTx :: HT.TxHash -> ClientM (Maybe HT.Tx)
fundingTx = AddrIndex.fetchBtcTxM

newtype TestDerive a = TestDerive (IO a)
    deriving (Functor, Applicative, Monad, R.MonadIO)

deriveTestFunding
    :: PC.ChanParams
    -> TestDerive (Maybe (AddrFunding, HT.Tx))
deriveTestFunding =
    fmap Just . TestDerive . Test.deriveMockFunding


