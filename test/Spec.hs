{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import MyPrelude
import TstPrelude
import TstUtil
import RBPCP.Handler.Conf
import PaymentChannel.Test
import qualified PaymentChannel                 as PC
import qualified RBPCP.Types                    as RBPCP
import qualified Network.Haskoin.Constants      as HCC
import qualified Network.Bitcoin.AddrIndex.API  as Addr
import qualified Control.Monad.Logger           as Log
import qualified Text.Show.Pretty               as Pretty


numMaxPayments :: Int
numMaxPayments = 100

maxOpenPrice :: BtcAmount
maxOpenPrice = 50000

logLevel :: Log.LogLevel
logLevel = Log.LevelInfo

testServerRootKey :: PC.RootPrv
testServerRootKey = PC.createRootPrv
    "4b52502262a65ab2f7f42e4d12484aef2aa89c3c2d7f2f842be16aa41af8024e"

main :: IO ()
main = HCC.switchToTestnet3 >> hspec spec

spec :: Spec
spec =
  describe "Payment channel" $
    around (withRbpcpServer testServerRootKey maxOpenPrice) $
      it "can be funded, opened, paid to, closed" $ \(testConf, servUrl, man) -> do
        let runServReq :: String -> ClientM a -> IO a
            runServReq = testRunReq (servUrl, man)

        -- * Fetch/check server funding info
        ArbitraryPubKeyC prvKey pubKey <- generate arbitrary
        now <- liftIO currentTime
        lockTime <- generate $ genLockTimeDate (scSettings testConf) now (24 * 7 * 52 :: Hour)
        let clientPK = RBPCP.Client pubKey
        fundInfo <- runServReq "FundingInfo" $ fundingInfo clientPK (toWord32 lockTime)
        Log.runStdoutLoggingT . Log.logInfoN . cs $ unlines
            [ "Received FundingInfo from server:" , Pretty.ppShow fundInfo ]
        cp <- either (error . show) return $ validFundingInfo (MkSendPubKey pubKey) lockTime fundInfo

        -- * Wait for blockchain funding
        let fundingAddr = getFundingAddress cp
        putStrLn $ unwords
              [ "Please pay at least"
              , show (fromIntegral $ RBPCP.fundingInfoOpenPrice fundInfo :: BtcAmount)
              , "to funding address " ++ showJsonStr fundingAddr
              ]
        (fundingTx, addrFundInfo) <- loopFundingWait testConf man fundInfo fundingAddr
        putStrLn $ unwords
              [ "Confirmation count reached. Funding value: "
              , show (fromIntegral $ Addr.asiValue addrFundInfo :: BtcAmount)
              ]
        -- Elegant solution: parse possible server error response and wait a bit if it's "insufficienct conf count"
        putStrLn "Waiting a bit to make sure server's blockchain-view is in sync with ours..."
        threadDelay 10000000

        -- * Open payment channel
        let txVoutPair = (fundingTx, Addr.asiFundingVout addrFundInfo)
            (clientChan, initPay) = either (error . show) id $
                channelWithInitialPayment prvKey lockTime txVoutPair fundInfo
        let fundingTxId = RBPCP.BtcTxId $ Addr.asiFundingTxId addrFundInfo
            fundingVout = Addr.asiFundingVout addrFundInfo
            sharedSecret = Just $ getSecret clientChan
        openPayRes <- runServReq "ChanOpen" $ chanOpen fundingTxId fundingVout sharedSecret
              (mkRbpcpPayment initPay)
        RBPCP.paymentResultValueReceived openPayRes `shouldBe` RBPCP.fundingInfoOpenPrice fundInfo
        putStrLn "Channel opened."

        -- * Make payments
        let fundingVal = Addr.asiValue addrFundInfo
            availableValue = fundingVal - fromIntegral (RBPCP.fundingInfoOpenPrice fundInfo)
        numPayments <- generate $ choose (0, numMaxPayments)
        --  Average payment value should be 2*availableValue
        --    (so some payment sessions overflow and others underflow)
        payAmountLst <- generate . vectorOf numPayments $
              choose (0 :: Word64 , ceiling $ availableValue `divRatio` numPayments * 2)
        let makePayment :: (ClientPayChanI BtcSig, [BtcAmount])
                        -> BtcAmount
                        -> IO (ClientPayChanI BtcSig, [BtcAmount])
            makePayment (!state, !payLst) val = do
                let (newState, payment, actualAmt) = createPaymentCapped state (Capped val)
                payRes <- runServReq "ChanPay" $
                    chanPay fundingTxId fundingVout sharedSecret (mkRbpcpPayment payment)
                RBPCP.paymentResultValueReceived payRes `shouldBe` fromIntegral actualAmt
                putStrLn $ "Payment sent. Value: " ++ show actualAmt
                return (newState, payLst ++ [actualAmt])
        (finalState, payValLst) <- foldM makePayment (clientChan,[]) (map fromIntegral payAmountLst :: [BtcAmount])

        -- * Close channel
        ArbitraryPubKeyAddress changeAddr <- generate arbitrary
        putStrLn $ unwords
              [ "Payments totalling"
              , show (sum payValLst)
              , "sent. Closing channel; withdrawing to"
              , showJsonStr changeAddr
              ]
        arbFee <- fromIntegral <$> generate (choose (0 :: Word64, fromIntegral $ channelValueLeft finalState))
        let (_, closingPayment, _) = createClosingPayment finalState changeAddr (arbFee :: BtcAmount)
        closeRes <- runServReq "ChanClose" $
              chanClose fundingTxId fundingVout sharedSecret (mkRbpcpPayment closingPayment)
        closeRes `shouldSatisfy` isJust . RBPCP.paymentResultSettlementTxid
