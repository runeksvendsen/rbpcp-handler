module RBPCP.Handler.FundingInfo where

--import           PayChanServer.Types
--import           PayChanServer.Util
--import qualified PayChanServer.Config.Types as Conf
import           PaymentChannel       (getDustLimit)


fundingInfoHandler :: PayChanConf m
                   => SendPubKey -> LockTimeDate -> m FundingInfo
fundingInfoHandler clientPK lockTime = do
    -- TODO: Get pubkey from BitcoinSigner
    serverPK <- confGet Conf.pubKey
    (Conf.ChanConf btcMinConf openPrice dustLimitT settlePeriod minDuratn) <- confGet Conf.chanConf
    let dustLimit = getVal dustLimitT
    let chanParams = MkChanParams clientPK serverPK lockTime
    return $ FundingInfo serverPK dustLimit (getFundingAddress chanParams)
             (getRedeemScript chanParams)
             (getVal openPrice) (getVal btcMinConf) (getVal settlePeriod) (getVal minDuratn)
     where getVal = Conf.getVal

