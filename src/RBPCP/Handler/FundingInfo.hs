module RBPCP.Handler.FundingInfo where

import RBPCP.Handler.Internal.Util
import Settings
import qualified RBPCP.Internal.Conf          as Conf
import qualified RBPCP.Types                  as RBPCP
import qualified PaymentChannel               as PC
import qualified ChanDB                       as DB
import qualified Servant.Server               as SS


fundingInfo ::
    RBPCP.Client PC.PubKey -> Word32 -> HandlerM a RBPCP.FundingInfo
fundingInfo (RBPCP.Client clientPk) lockTime = do
    let handleErr :: (MonadError SS.ServantErr m, PC.IsPayChanError e) => Either e a -> m a
        handleErr = either (throwUserError . PC.mkChanErr) return
    lockTimeDate <- handleErr $ PC.parseLockTime lockTime
    serverPk     <- getCurrentPubKey
    let chanParams = PC.ChanParams (PC.MkSendPubKey clientPk) serverPk lockTimeDate
    handleErr =<< PC.hasMinimumDuration serverSettings lockTimeDate
    return $ mkFundingInfo serverSettings chanParams

getCurrentPubKey :: HandlerM dbH PC.RecvPubKey
getCurrentPubKey = do
  getter <- asks Conf.hcCurrPubKey
  handleErrorE =<< fmap DB.kaiPubKey <$> liftIO getter

mkFundingInfo :: PC.ServerSettings -> PC.ChanParams -> RBPCP.FundingInfo
mkFundingInfo PC.ServerSettings{..} chanParams =
    RBPCP.FundingInfo
         { RBPCP.fundingInfoServerPubkey               = RBPCP.Server . PC.getPubKey $ PC.getRecvPubKey chanParams
         , RBPCP.fundingInfoDustLimit                  = fromIntegral serverConfDustLimit
         , RBPCP.fundingInfoFundingAddressCopy         = PC.getFundingAddress chanParams
         , RBPCP.fundingInfoOpenPrice                  = fromIntegral serverConfOpenPrice
         , RBPCP.fundingInfoFunding_tx_min_conf        = confMinBtcConf
         , RBPCP.fundingInfoSettlement_period_hours    = fromIntegral serverConfSettlePeriod
         , RBPCP.fundingInfoMin_duration_hours         = fromIntegral serverConfMinDuration
         }
