module TstUtil
( module TstUtil
, module X
)
where

import TstPrelude as X
import Servant
import qualified RBPCP.Internal.Manager     as Man
import qualified Servant.Client             as SC
import qualified Network.HTTP.Types.Status  as Stat
import Data.Function                        (fix)
import Control.Concurrent                   (threadDelay)
import Control.Monad                        (unless)
import RBPCP.Handler.Conf
import qualified PaymentChannel.Test        as PC
import Data.List                                (sortOn)
import qualified RBPCP.App                      as App
import qualified BitcoinSigner.App              as SignApp
import qualified RBPCP.Handler.Internal.Funding as Funding
import qualified RBPCP.Types                    as RBPCP
import qualified Control.Concurrent.Async       as Async
import qualified Network.Haskoin.Crypto         as HC
import qualified Network.Haskoin.Transaction    as HT
import qualified Network.Bitcoin.AddrIndex.API  as Addr
import qualified Control.Monad.Logger as Log


genServerSettings :: (PC.BtcAmount,PC.BtcAmount) -> Gen PC.ServerSettings
genServerSettings (minOpenPrice, maxOpenPrice) = do
    arbOpenPrice <- choose (fromIntegral minOpenPrice, fromIntegral maxOpenPrice :: Word64)
    return PC.ServerSettings
        { PC.serverConfDustLimit     = 6000
        , PC.serverConfSettlePeriod  = PC.MkHour 12
        , PC.serverConfMinDuration   = PC.MkHour 48
        , PC.serverConfOpenPrice     = fromIntegral arbOpenPrice
        }

mkTestConf :: AddrIndexServerUrl -> PC.ServerSettings -> ServerConf
mkTestConf addrIndexServer serverSettings = ServerConf
    { scSettings      = serverSettings
    , scMinBtcConf    = 1
    , scProofServer   = addrIndexServer
    , scBitcoinSigner = BaseUrl Http  "localhost" 8081 ""
    }

loopFundingWait
    :: ServerConf
    -> ReqMan
    -> RBPCP.FundingInfo
    -> HC.Address
    -> IO (HT.Tx, Addr.AddressFundingInfo)
loopFundingWait servCfg man fundInfo fundingAddr = do
    _ <- waitForFunding fundingAddr
    putStrLn "Found tx paying to funding address. Waiting for confirmations..."
    fix $ \loop -> do
        addrFundInfo <- waitForFunding fundingAddr
        if fromIntegral (Addr.asiConfs addrFundInfo) < RBPCP.fundingInfoFundingTxMinConf fundInfo
            then waitABit >> loop
            else do
                txM <- testRunReq (confProofServer, man) "confProof" $
                            Funding.fetchBtcTxM (Addr.asiFundingTxId addrFundInfo)
                let tx = fromMaybe (error "No tx with the txid returned by 'unspentOuts'") txM
                return (tx, addrFundInfo)
  where
    confProofServer = proofServerUrl $ scProofServer servCfg
    waitABit = threadDelay 10000000
    waitForFunding addr = fix $ \loop -> do
        addrFundInfoL <- testRunReq (confProofServer, man) "addrFundInfo" $ Funding.unspentOuts addr
        if null addrFundInfoL
            then waitABit >> loop
            else return $ last $ sortOn Addr.asiConfs addrFundInfoL

withRbpcpServer
    :: Log.LogLevel
    -> (PC.RootPrv,SignApp.ServerConf)
    -> (PC.BtcAmount,PC.BtcAmount)
    -> ((ServerConf, BaseUrl, ReqMan) -> IO ())
    -> IO ()
withRbpcpServer logLvl (rootKey,signConf) minMaxOpenPrice f = do
    let serverUrl = BaseUrl Http "localhost" 8080 ""
    servSettings <- generate (genServerSettings minMaxOpenPrice)
    let cfg@ServerConf{..} = mkTestConf (SignApp.scBitcoinServer signConf) servSettings
    man <- mkReqMan
    tidSignServ <- Async.async $ SignApp.signingAppMain
          (fromIntegral $ baseUrlPort scBitcoinSigner) signConf rootKey
    waitForServer man scBitcoinSigner   -- # Waiting for signing-server to start...
    tidRbpcpServer <- Async.async $ App.testApp logLvl (fromIntegral $ baseUrlPort serverUrl) cfg
    waitForServer man serverUrl         -- # Waiting for RBPCP-server to start...
    f (cfg, serverUrl, man)
    Async.cancel tidRbpcpServer
    Async.cancel tidSignServ

type DummyApi = "dummy_path_shouldnt_exist" :> Get '[JSON] String

dummyClientM :: SC.ClientM String
dummyClientM = SC.client (Proxy :: Proxy DummyApi)

waitForServer :: Man.ReqMan -> BaseUrl -> IO ()
waitForServer man servUrl = fix $ \loop -> do
    resE <- runReq (servUrl, man) dummyClientM
    case resE of
        Left (SC.ConnectionError _) -> threadDelay 100000 >> loop
        Left (SC.FailureResponse status _ body) ->
            unless (status == Stat.status404) $
                error $ "waitForServer: unexpected failure: " ++ show (status,body)
        Left x -> error $ "waitForServer: unexpected error response: " ++ show x
        Right _ -> error "waitForServer: unexpected success. dummy path exists."

