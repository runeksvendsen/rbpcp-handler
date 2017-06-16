module Main where

import MyPrelude
import TstPrelude
import qualified Settings
import qualified RBPCP.App                      as App
import qualified RBPCP.Handler.Internal.Funding as Funding
import qualified RBPCP.Api                      as API
import qualified RBPCP.Types                    as RBPCP
import qualified Network.Haskoin.Crypto         as HC
import qualified Network.Bitcoin.AddrIndex.API  as Addr

import PaymentChannel.Test
import Network.Haskoin.Test
import Test.Hspec
import Test.QuickCheck
import Servant.Client
import Data.Function                            (fix)
import Data.List                                (sortOn)


testServerUrl :: BaseUrl
testServerUrl = BaseUrl Http "localhost" 8080 ""

testBitcoinSignerUrl :: BaseUrl
testBitcoinSignerUrl = BaseUrl Http "localhost" 8081 ""

numMaxPayments :: Int
numMaxPayments = 10000


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Payment channel" $
    around withManager $
      it "Can be funded, opened, paid to, closed" $ \man -> do
        -- * Fetch/check server funding info
        ArbitraryPubKeyC prvKey pubKey <- generate arbitrary
        now <- liftIO currentTime
        lockTime <- generate $ genLockTimeDate Settings.serverSettings now (24 * 7 * 52 :: Hour)
        let clientPK = RBPCP.Client pubKey
        fundInfo <- testRunReq (testServerUrl, man) $ fundingInfo clientPK (toWord32 lockTime)
        cp <- either (error . show) return $ validFundingInfo clientPK lockTime fundInfo

        -- * Wait for blockchain funding
        let fundingAddr = getFundingAddress cp
        putStrLn ("Please pay to funding address " ++ show fundingAddr)
        (fundingProof, addrFundInfo) <- loopFundingWait man fundInfo fundingAddr
        putStrLn "Confirmation count reached."
        -- Elegant solution: parse possible server error response and wait a bit if it's "insufficienct conf count"
        putStrLn "Waiting a bit to make sure server's blockchain-view is in sync with ours..."
        threadDelay 10000000

        -- * Open payment channel
        let txVoutPair = (Addr.proof_tx_data fundingProof, Addr.asiFundingVout addrFundInfo)
            (clientChan, initPay) = either (error . show) id $
                channelWithInitialPayment prvKey lockTime txVoutPair fundInfo
        let fundingTxId = RBPCP.BtcTxId $ Addr.asiFundingTxId addrFundInfo
            fundingVout = Addr.asiFundingVout addrFundInfo
            sharedSecret = Just $ toHash $ getSecret clientChan
        openPayRes <- testRunReq (testServerUrl, man) $ chanOpen fundingTxId fundingVout sharedSecret
              (mkRbpcpPayment initPay)
        RBPCP.paymentResult_value_received openPayRes `shouldBe` RBPCP.fundingInfoOpenPrice fundInfo

        -- * Make payments
        numPayments  <- generate $ choose (0, numMaxPayments)
        payAmountLst <- generate $ vectorOf numPayments arbitrary
        let makePayment :: ClientPayChanI BtcSig -> BtcAmount -> IO (ClientPayChanI BtcSig)
            makePayment state val = do
                let (newState, payment, actualAmt) = createPayment state (Capped val)
                payRes <- testRunReq (testServerUrl, man) $
                    chanPay fundingTxId fundingVout sharedSecret (mkRbpcpPayment payment)
                RBPCP.paymentResult_value_received payRes `shouldBe` fromIntegral actualAmt
                return newState
        finalState <- foldM makePayment clientChan (payAmountLst :: [BtcAmount])

        -- * Close channel
        arbFee <- fromIntegral <$> generate (choose (0 :: Word64, fromIntegral $ channelValueLeft finalState))
        ArbitraryAddress changeAddr <- generate arbitrary
        let (_, closingPayment, actualFee) = createClosingPayment
              finalState changeAddr (arbFee :: BtcAmount)
        actualFee `shouldBe` arbFee
        void $ testRunReq (testServerUrl, man) $
              chanClose fundingTxId fundingVout sharedSecret (mkRbpcpPayment closingPayment)

loopFundingWait :: Manager -> RBPCP.FundingInfo -> HC.Address -> IO (Addr.FundingProof, Addr.AddressFundingInfo)
loopFundingWait man fundInfo fundingAddr = do
    _ <- waitForFunding fundingAddr
    putStrLn "Found tx paying to funding address. Waiting for confirmations..."
    fix $ \loop -> do
        addrFundInfo <- waitForFunding fundingAddr
        if fromIntegral (Addr.asiConfs addrFundInfo) < RBPCP.fundingInfoFunding_tx_min_conf fundInfo
            then waitABit >> loop
            else do
                proofM <- testRunReq (Settings.confProofServer, man) $
                            Funding.fundingProofM (Addr.asiFundingTxId addrFundInfo)
                let proof = fromMaybe (error "No tx with txid returned by 'unspentOuts'") proofM
                return (proof, addrFundInfo)
  where
    waitABit = threadDelay 10000000
    waitForFunding addr = fix $ \loop -> do
        addrFundInfoL <- testRunReq (Settings.confProofServer, man) $ Funding.unspentOuts addr
        if null addrFundInfoL
            then waitABit >> loop
            else return $ last $ sortOn Addr.asiConfs addrFundInfoL

withManager :: (Manager -> IO ()) -> IO ()
withManager f = do
    man <- mkManager
    f man

mkRbpcpPayment :: SignedPayment -> RBPCP.Payment
mkRbpcpPayment p = RBPCP.Payment (toPaymentData p) ""

fundingInfo :: RBPCP.Client RBPCP.PubKey -> RBPCP.BLT -> ClientM RBPCP.FundingInfo
fundingInfo = client (Proxy :: Proxy API.FundInfo)

chanOpen :: RBPCP.BtcTxId -> RBPCP.Vout -> Maybe HC.Hash256 -> RBPCP.Payment -> ClientM RBPCP.PaymentResult
chanOpen = client (Proxy :: Proxy API.ChanOpen)

chanPay :: RBPCP.BtcTxId -> RBPCP.Vout -> Maybe HC.Hash256 -> RBPCP.Payment -> ClientM RBPCP.PaymentResult
chanPay = client (Proxy :: Proxy API.ChanPay)

chanClose :: RBPCP.BtcTxId -> RBPCP.Vout -> Maybe HC.Hash256 -> RBPCP.Payment -> ClientM RBPCP.PaymentResult
chanClose = client (Proxy :: Proxy API.ChanClose)
