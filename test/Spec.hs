{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import MyPrelude
import TstPrelude
import TstUtil
import RBPCP.Handler.Conf
import PaymentChannel.Test
import qualified RBPCP.Handler.Internal.Funding.Test as FundTest
import qualified PaymentChannel                 as PC
import qualified RBPCP.Types                    as RBPCP
import qualified Network.Haskoin.Transaction    as HT
import qualified Network.Haskoin.Crypto         as HC
import qualified Network.Haskoin.Constants      as HCC
import qualified Network.Bitcoin.AddrIndex.API  as Addr
import qualified Control.Monad.Logger           as Log
import qualified Text.Show.Pretty               as Pretty
import qualified Network.Bitcoin.AddrIndex.Types as AI
import qualified BitcoinSigner.App              as SignApp
import qualified Util.Bitcoin.CoreFee           as Fee



numMaxPayments :: Int
numMaxPayments = 100

numThreads :: Int
numThreads = 50

maxOpenPrice :: BtcAmount
maxOpenPrice = 50000

minOpenPrice :: BtcAmount
minOpenPrice = 10000

changeAddress :: HC.Address
changeAddress =
    if HCC.getNetwork == HCC.prodnet
        then "1JAtvgYXjKQq18rbDmFvhFC2nh7PZwQtJ"
        else "mkkHB1Z3pCQQMiuXrZ82RsHectcB1ttK1k"

signConf :: SignApp.ServerConf
signConf = SignApp.ServerConf (Fee.BlockDelay 10) addrIndexServer

addrIndexServer :: AI.AddrIndexServerUrl
addrIndexServer =
    AI.AddrIndexServerUrl
        (BaseUrl Https "blockchain.runeks.me" 443 "")
        (BaseUrl Https "blockchaintest.runeks.me" 443 "")

logLevel :: Log.LogLevel
logLevel = Log.LevelError

btcNetwork :: DisabledNet
btcNetwork = DisabledNet

testServerRootKey :: PC.RootPrv
testServerRootKey = PC.createRootPrv
    "4b52502262a65ab2f7f42e4d12484aef2aa89c3c2d7f2f842be16aa41af8024e"

main :: IO ()
main = do
    HCC.switchToTestnet3
    hspec (specDerive >> spec)

specDerive :: Spec
specDerive =
    describe "Mock funding tx" $
        it "can be deterministically derived from any ChanParams" $ do
            cp <- generate arbitrary
            numReplicas <- generate $ choose (2,100)
            resL <- replicateM numReplicas $ FundTest.deriveMockFunding cp
            resL `shouldSatisfy` all (== head resL)

spec :: Spec
spec =
    describe "Payment channel" $
        around (withRbpcpServer btcNetwork logLevel (testServerRootKey,signConf) (minOpenPrice,maxOpenPrice)) $
            it "can be funded, opened, paid to, closed" $ \serverArgs@(testConf, _, _) -> do
                clientArgL <- replicateM numThreads (genClientArgs testConf)
                numPayL <- doConcurrently (fundOpenPayClose serverArgs) clientArgL
                putStrLn $ unwords [ "Done!", show $ sum (map length numPayL), "payments executed." ]

genClientArgs :: ServerConf chain -> IO (HC.PrvKeyC, HC.PubKeyC, LockTimeDate)
genClientArgs testConf = do
    -- Generate client pubkey + channel expiration date
    ArbitraryPubKeyC prvKey pubKey  <- generate arbitrary
    now <- liftIO currentTime
    lockTime <- generate $ genLockTimeDate (scSettings testConf) now (24 * 7 * 52 :: Hour)
    return (prvKey, pubKey, lockTime)

fundOpenPayClose :: BlockchainRun chainM chain
                 => (ServerConf chain, BaseUrl, ReqMan)
                 -> (HC.PrvKeyC, HC.PubKeyC, LockTimeDate)
                 -> IO [BtcAmount]
fundOpenPayClose (testConf, servUrl, man) (prvKey, pubKey, lockTime) = do
    -- GET FundingInfo + wait for funding
    (fundInfo, chanParams)    <- fetchServerFundingInfo runServReq lockTime pubKey
    (fundingTx, addrFundInfo) <- waitBlockchainFunding man testConf chanParams fundInfo
    let fundingVal = fromIntegral (Addr.asiValue addrFundInfo)
        dustLimit = fromIntegral $ RBPCP.fundingInfoDustLimit fundInfo
        openPrice  = fromIntegral $ RBPCP.fundingInfoOpenPrice fundInfo
    if fundingVal < openPrice + dustLimit
        then do
            putStrLn $ unwords [ "Insufficient funding value:", show fundingVal ++ "."
                               , "Open price:", show openPrice ++ "."
                               , "Dust limit:", show dustLimit ]
            return []
        else do
            putStrLn $ "Channel funded. Value: " ++ show fundingVal
            -- * Create client state + initial payment
            let fundingVout = Addr.asiFundingVout addrFundInfo
                sharedSecret = Just $ getSecret clientChan
                (clientChan, initPay) = either (error . show) id $
                    channelWithInitialPayment prvKey lockTime (fundingTx,fundingVout) fundInfo
            -- Util
            let apiRun = rbpcpApi (RBPCP.BtcTxId $ HT.txHash fundingTx) fundingVout sharedSecret
            -- * Open payment channel
            PC.getFundingAmount clientChan `shouldBe` fundingVal
            openChan runServReq apiRun fundInfo initPay
            -- * Make payments
            (finalState, payValL) <- makePayments runServReq apiRun fundInfo clientChan
            -- * Close channel
            closeChannel runServReq apiRun finalState
            return payValL
  where
    runServReq :: String -> ClientM a -> IO a
    runServReq = testRunReq (servUrl, man)

fetchServerFundingInfo
    :: (String -> ClientM RBPCP.FundingInfo -> IO RBPCP.FundingInfo)
    -> LockTimeDate
    -> HC.PubKeyC
    -> IO (RBPCP.FundingInfo, ChanParams)
fetchServerFundingInfo runServReq lockTime pubKey = do
    fundInfo <- runServReq "FundingInfo" $ fundingInfo clientPK (toWord32 lockTime)
--    Log.runStdoutLoggingT . Log.logInfoN . cs $ unlines
--        [ "Received FundingInfo from server:" , Pretty.ppShow fundInfo ]
    let cpE = validFundingInfo (MkSendPubKey pubKey) lockTime fundInfo
    cpE `shouldSatisfy` isRight
    return (fundInfo, either (error . show) id cpE)
  where
    clientPK = RBPCP.Client pubKey

waitBlockchainFunding
    :: BlockchainRun chainM chain
    => ReqMan
    -> ServerConf chain
    -> ChanParams
    -> RBPCP.FundingInfo
    -> IO (HT.Tx, AI.AddressFundingInfo)
waitBlockchainFunding man testConf chanParams fundInfo = do
--    putStrLn $ unwords
--          [ "Please pay at least"
--          , show (fromIntegral $ RBPCP.fundingInfoOpenPrice fundInfo :: BtcAmount)
--          , "to funding address " ++ showJsonStr (PC.getFundingAddress chanParams)
--          ]
    (fundingTx, addrFundInfo) <- loopFundingWait testConf man fundInfo chanParams
    -- Elegant solution: parse possible server error response and wait a bit if it's "insufficienct conf count"
--    putStrLn "Waiting a bit to make sure server's blockchain-view is in sync with ours..."
--    threadDelay 10000000
    return (fundingTx, addrFundInfo)

openChan runServReq apiRun fundInfo initPay = do
    openPayRes <- runServReq "ChanOpen" $ apiRun ChanOpen initPay
    RBPCP.paymentResultValueReceived openPayRes `shouldBe` RBPCP.fundingInfoOpenPrice fundInfo

makePayments runServReq apiRun fundInfo clientChan = do
    numPayments <- generate $ choose (0, numMaxPayments)
    payAmountLst <- generate . vectorOf numPayments $
          choose (0 :: Word64 , ceiling $ 2*(availableValue `divRatio` numPayments))
    foldM makePayment (clientChan,[]) (map fromIntegral payAmountLst :: [BtcAmount])
  where
    fundingVal = PC.getFundingAmount clientChan
    availableValue = fundingVal - fromIntegral (RBPCP.fundingInfoOpenPrice fundInfo)
    makePayment :: (ClientPayChan, [BtcAmount])
                -> BtcAmount
                -> IO (ClientPayChan, [BtcAmount])
    makePayment (!state, !payLst) val = do
        let (newState, payment, actualAmt) = createPaymentCapped state (Capped val)
        payRes <- runServReq "ChanPay" $ apiRun ChanPay payment
        RBPCP.paymentResultValueReceived payRes `shouldBe` fromIntegral actualAmt
--        putStrLn $ "Payment sent. Value: " ++ show actualAmt
        return (newState, actualAmt : payLst)

closeChannel runServReq apiRun clientChan = do
--    putStrLn $ unwords [ "Closing channel; withdrawing to" , showJsonStr changeAddress ]
    arbFee <- fromIntegral <$> generate (choose (0 :: Word64, fromIntegral $ channelValueLeft clientChan))
    let (_, closingPayment, _) = createClosingPayment clientChan changeAddress (arbFee :: BtcAmount)
    closeRes <- runServReq "ChanClose" $ apiRun ChanClose closingPayment
    closeRes `shouldSatisfy` isJust . RBPCP.paymentResultSettlementTxid






