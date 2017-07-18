module RBPCP.Handler.Internal.Funding.Test
( deriveMockFunding
)
where

import MyPrelude
import qualified PaymentChannel.Test            as PC
import RBPCP.Handler.Internal.BtcAddrIndex      (AddressFundingInfo(..))
import qualified Network.Haskoin.Transaction    as HT
import qualified Test.QuickCheck                as QC
import qualified Test.QuickCheck.Random         as QC
import qualified Test.QuickCheck.Gen            as QC
import qualified Data.Serialize                 as Bin
import qualified Network.Haskoin.Crypto         as HC
import qualified Crypto.Util                    as Cryp
import Data.Ratio


maxFundingVal :: PC.BtcAmount
maxFundingVal = 1000000 -- round (1000e8 :: Ratio Word64)

-- | Deterministally derive a mock 'Addr.AddressFundingInfo' so server and client
--    can agree on a set of "fake" test funding parameters, in order to be able to
--    ignore the Bitcoin network for testing purposes.
deriveMockFunding
    :: PC.ChanParams
    -> IO (AddressFundingInfo, HT.Tx)
deriveMockFunding cp =
    return $ detGen qcGen intSeed
  where
    intSeed :: Int
    intSeed = fromIntegral $ rndSeed `mod` 2^10
    rndSeed :: Integer
    rndSeed = Cryp.bs2i . Bin.encode . HC.hash256 . Bin.encode $ cp
    qcGen   =   QC.variantQCGen rndSeed (QC.mkQCGen intSeed)
    (QC.MkGen detGen) = deriveMockFundingGen cp

deriveMockFundingGen
    :: PC.ChanParams
    -> QC.Gen (AddressFundingInfo, HT.Tx)
deriveMockFundingGen cp@PC.ChanParams{..} = do
    fundVal <- QC.choose (1, fromIntegral maxFundingVal)
    (fundVout,fundTx) <- PC.arbitraryFundingTx cp (fromIntegral fundVal)
    let afi = AddressFundingInfo
                { asiDestAddress  = PC.getFundingAddress cp
                , asiFundingTxId  = HT.txHash fundTx
                , asiFundingVout  = fundVout
                , asiConfs        = fromIntegral (maxBound :: Word32)
                , asiValue        = fromIntegral (fundVal :: Word64)
                }
    return (afi, fundTx)



