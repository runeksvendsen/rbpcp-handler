module RBPCP.Handler.FundingInfo where

import RBPCP.Handler.Internal.Util
import Settings
import qualified Conf
import qualified RBPCP.Types                  as RBPCP
import qualified PaymentChannel               as PC
import qualified ChanDB                       as DB


fundingInfo ::
    RBPCP.Client PC.PubKey -> Word32 -> HandlerM a RBPCP.FundingInfo
fundingInfo (RBPCP.Client clientPk) lockTime = do
    serverPk <- getCurrentPubKey
    lockTimeDate <- either (throwUserError . PC.mkChanErr) return $ PC.parseLockTime lockTime
    let chanParams = PC.MkChanParams (PC.MkSendPubKey clientPk) serverPk lockTimeDate
    return $ mkFundingInfo chanParams  

getCurrentPubKey :: HandlerM dbH PC.RecvPubKey
getCurrentPubKey = do
  getter <- asks Conf.hcCurrPubKey
  handleErrorE =<< fmap DB.kaiPubKey <$> liftIO getter


mkFundingInfo :: PC.ChanParams -> RBPCP.FundingInfo
mkFundingInfo chanParams =
    RBPCP.FundingInfo
         { RBPCP.fundingInfoServerPubkey               = RBPCP.Server . PC.getPubKey $ PC.getRecvPubKey chanParams
         , RBPCP.fundingInfoDustLimit                  = fromIntegral PC.getDustLimit
         , RBPCP.fundingInfoFundingAddressCopy         = PC.getFundingAddress chanParams
         , RBPCP.fundingInfoOpenPrice                  = fromIntegral confOpenPrice
         , RBPCP.fundingInfoFunding_tx_min_conf        = confMinBtcConf
         , RBPCP.fundingInfoSettlement_period_hours    = fromIntegral $ unTagged PC.getSettlePeriod
         , RBPCP.fundingInfoMin_duration_hours         = confMinDurationHours
         }


