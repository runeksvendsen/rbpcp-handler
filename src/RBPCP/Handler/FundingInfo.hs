module RBPCP.Handler.FundingInfo where

import RBPCP.Handler.Internal.Util
import qualified RBPCP.Handler.Conf          as Conf
import qualified RBPCP.Types                  as RBPCP
import qualified PaymentChannel               as PC
import qualified ChanDB                       as DB
import qualified Servant.Server               as SS


fundingInfo ::
    RBPCP.Client PC.PubKey -> Word32 -> HandlerM a RBPCP.FundingInfo
fundingInfo (RBPCP.Client clientPk) lockTime = do
    let handleErr :: (MonadError SS.ServantErr m, PC.IsPayChanError e) => Either e a -> m a
        handleErr = either (throwUserError . PC.mkChanErr) return
    serverConf   <- asks hcServerConf
    lockTimeDate <- handleErr $ PC.parseLockTime lockTime

    rootPub <- asks hcPubKey
    let (chanParams,_) = PC.deriveRecvPub rootPub $ PC.UserParams (PC.MkSendPubKey clientPk) lockTimeDate

--    serverPk     <- getCurrentPubKey
--    let chanParams = PC.ChanParams  serverPk lockTimeDate
    handleErr =<< PC.hasMinimumDuration (scSettings serverConf) lockTimeDate
    return $ mkFundingInfo serverConf chanParams

--getCurrentPubKey :: HandlerM dbH PC.RecvPubKey
--getCurrentPubKey = do
--  getter <- asks Conf.hcCurrPubKey
--  handleErrorE =<< fmap DB.kaiPubKey <$> liftIO getter

mkFundingInfo :: ServerConf -> PC.ChanParams -> RBPCP.FundingInfo
mkFundingInfo (ServerConf PC.ServerSettings{..} confMinBtcConf _ _) chanParams =
    RBPCP.FundingInfo
         { RBPCP.fundingInfoServerPubkey          = RBPCP.Server . PC.getPubKey $ PC.getRecvPubKey chanParams
         , RBPCP.fundingInfoDustLimit             = fromIntegral serverConfDustLimit
         , RBPCP.fundingInfoFundingAddressCopy    = PC.getFundingAddress chanParams
         , RBPCP.fundingInfoOpenPrice             = fromIntegral serverConfOpenPrice
         , RBPCP.fundingInfoFundingTxMinConf      = confMinBtcConf
         , RBPCP.fundingInfoSettlementPeriodHours = fromIntegral serverConfSettlePeriod
         , RBPCP.fundingInfoMinDurationHours      = fromIntegral serverConfMinDuration
         }
