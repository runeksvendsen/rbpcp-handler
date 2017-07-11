module RBPCP.Handler.Internal.Funding.Test where

import MyPrelude
import qualified PaymentChannel.Test            as PC
import RBPCP.Handler.Internal.BtcAddrIndex      (AddressFundingInfo(..))
import qualified Network.Haskoin.Transaction    as HT
import qualified Test.QuickCheck                as QC
import qualified Data.Serialize                 as Bin
import qualified Network.Haskoin.Crypto         as HC
import qualified Crypto.Util                    as Cryp
import qualified RBPCP.Types                    as RBPCP


 {-

data AddressFundingInfo = AddressFundingInfo {
    afiDestAddress  ::  HC.Address
   ,afiFundingTxId  ::  HT.TxHash
   ,afiFundingVout  ::  Word32
   ,afiConfs        ::  Integer
   ,afiValue        ::  Integer
} deriving (Eq, Show, Generic)
  -}


-- | Deterministally derive a mock 'Addr.AddressFundingInfo' so server and client
--    can agree on a set of "fake" test funding parameters, in order to be able to
--    ignore the Bitcoin network for testing purposes.
deriveMockAddrFundingInfo
    :: PC.ChanParams
    -> RBPCP.FundingInfo
    -> QC.Gen AddressFundingInfo
deriveMockAddrFundingInfo cp@PC.ChanParams{..} RBPCP.FundingInfo{..} = detGen $ do
    fundVal <- QC.choose (fundingInfoOpenPrice, fromIntegral (maxBound :: PC.BtcAmount))
    confs   <- QC.choose (fundingInfoFundingTxMinConf, maxBound)
    (fundVout,fundTx) <- PC.arbitraryFundingTx cp (fromIntegral fundVal)
    return AddressFundingInfo
        { asiDestAddress  = PC.getFundingAddress cp
        , asiFundingTxId  = HT.txHash fundTx
        , asiFundingVout  = fundVout
        , asiConfs        = fromIntegral confs
        , asiValue        = fromIntegral (fundVal :: Word64)
        }
  where
    rndSeed     = Cryp.bs2i . Bin.encode . HC.hash256 . Bin.encode $ cp
    detGen      = QC.variant rndSeed


{-
arbitraryFundingTx
    :: ChanParams
    -> BtcAmount        -- ^ Funding amount
    -> Gen (Word32, Tx)
 -}

