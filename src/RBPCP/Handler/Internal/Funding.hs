{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RBPCP.Handler.Internal.Funding
( module RBPCP.Handler.Internal.Funding
, module X
)
where

import MyPrelude
import Network.HTTP.Types.Status                  (Status(..))
import Servant.Client
import RBPCP.Handler.Internal.BtcAddrIndex        as X
import Network.Bitcoin.AddrIndex.API              ( PublishTx, UnspentOuts, TxOutProof
                                                  , Addr(..), PushTxReq(..)
                                                  , AddressFundingInfo(..), FundingProof
                                                  )
import qualified PaymentChannel                       as PC
import qualified RBPCP.Handler.Internal.BtcAddrIndex  as AddrIndex
import qualified Network.Haskoin.Transaction          as HT
import qualified Network.Haskoin.Crypto               as HC
import qualified Network.Haskoin.Constants            as HCC
import qualified Control.Monad.Reader                 as R
import qualified Control.Monad.Logger                 as Log


newtype MonadBitcoin a = MonadBitcoin
    { runBtcAddrIndex :: Log.LoggingT (R.ReaderT AddrIndexConf IO) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , R.MonadReader AddrIndexConf
        , Log.MonadLogger
        , R.MonadIO
        )

data AddrIndexConf
  = AddrIndexConf
  { aiscLivenetServer :: BaseUrl
  , aiscTestnetServer :: BaseUrl
  }

runFunding :: MonadIO m => AddrIndexConf -> MonadBitcoin a -> m a
runFunding cfg mf =
    R.liftIO $ R.runReaderT (Log.runStdoutLoggingT $ runBtcAddrIndex mf) cfg


unspentFundingOuts :: PC.ChanParams -> MonadBitcoin [AddressFundingInfo]
unspentFundingOuts cp = undefined -- AddrIndex.unspentOuts


fundingTx :: PC.ChanParams -> HT.TxHash -> MonadBitcoin (Maybe HT.Tx)
fundingTx cp = undefined -- AddrIndex.fetchBtcTxM


publishSettleTx :: HT.Tx -> MonadBitcoin ()
publishSettleTx tx = undefined -- AddrIndex.publishTx
