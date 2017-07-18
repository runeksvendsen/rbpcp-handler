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
import Control.Monad                        (unless, replicateM_)
import RBPCP.Handler.Conf
import qualified PaymentChannel.Test        as PC
import Data.List                                (sortOn)
import qualified RBPCP.App                      as App
import qualified BitcoinSigner.App              as SignApp
import qualified RBPCP.Handler.Internal.Blockchain as Funding
import qualified RBPCP.Types                    as RBPCP
import qualified Control.Concurrent.Async       as Async
import qualified Network.Haskoin.Crypto         as HC
import qualified Network.Haskoin.Transaction    as HT
import qualified Network.Bitcoin.AddrIndex.API  as Addr
import qualified Control.Monad.Logger as Log

timesDo :: Monad m => Int -> m a -> m [a]
timesDo = replicateM

doConcurrently :: (a -> IO b) -> [a] -> IO [b]
doConcurrently = Async.mapConcurrently

withRbpcpServer
    :: forall chainM chain.
       BlockchainRun chainM chain
    => chain
    -> Log.LogLevel
    -> (PC.RootPrv,SignApp.ServerConf)
    -> (PC.BtcAmount,PC.BtcAmount)
    -> ((ServerConf chain, BaseUrl, ReqMan) -> IO ())
    -> IO ()
withRbpcpServer chain logLvl (rootKey,signConf) minMaxOpenPrice f = do
    let serverUrl = BaseUrl Http "localhost" 8080 ""
    servSettings <- generate (genServerSettings minMaxOpenPrice)
    let cfg@ServerConf{..} = mkTestConf chain logLvl (SignApp.scBitcoinServer signConf) servSettings
    man <- mkReqMan
    tidSignServ <- Async.async $ SignApp.signingAppMain
          (fromIntegral $ baseUrlPort scBitcoinSigner) signConf rootKey
    waitForServer man scBitcoinSigner   -- # Waiting for signing-server to start...
    tidRbpcpServer <- Async.async $ App.testApp logLvl (fromIntegral $ baseUrlPort serverUrl) cfg
    waitForServer man serverUrl         -- # Waiting for RBPCP-server to start...
    f (cfg, serverUrl, man)
    Async.cancel tidRbpcpServer
    Async.cancel tidSignServ

genServerSettings :: (PC.BtcAmount,PC.BtcAmount) -> Gen PC.ServerSettings
genServerSettings (minOpenPrice, maxOpenPrice) = do
    arbOpenPrice <- choose (fromIntegral minOpenPrice, fromIntegral maxOpenPrice :: Word64)
    return PC.ServerSettings
        { PC.serverConfDustLimit     = 6000
        , PC.serverConfSettlePeriod  = PC.MkHour 12
        , PC.serverConfMinDuration   = PC.MkHour 48
        , PC.serverConfOpenPrice     = fromIntegral arbOpenPrice
        }

mkTestConf :: forall chain. chain -> Log.LogLevel -> AddrIndexServerUrl -> PC.ServerSettings -> ServerConf chain
mkTestConf chain logLvl addrIndexServer serverSettings = ServerConf
    { scSettings      = serverSettings
    , scMinBtcConf    = 1
    , scBitcoinSigner = BaseUrl Http  "localhost" 8081 ""
    , scBlockchainCfg = chainConf
    }
  where
    chainConf = BlockchainConf addrIndexServer logLvl chain


loopFundingWait
    :: forall chain chainM.
       BlockchainRun chainM chain
    => ServerConf chain
    -> ReqMan
    -> RBPCP.FundingInfo
    -> PC.ChanParams
    -> IO (HT.Tx, Addr.AddressFundingInfo)
loopFundingWait servCfg man fundInfo cp =
    fix $ \loop -> do
        addrFundInfoM <- either (error . show) return
            =<< runBlockchain man (scBlockchainCfg servCfg :: BlockchainConf chain)
                                  (getChannelFunding cp filterResults)
        case addrFundInfoM of
            Nothing -> waitABit >> loop
            Just (addrFundInfo,tx) ->
                if fromIntegral (Addr.asiConfs addrFundInfo) < RBPCP.fundingInfoFundingTxMinConf fundInfo
                    then waitABit >> loop
                    else return (tx,addrFundInfo)
  where
    waitABit = threadDelay 10000000
    filterResults = listToMaybe . reverse . sortOn Addr.asiValue

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

