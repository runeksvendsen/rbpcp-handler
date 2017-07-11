module TstPrelude
( module TstPrelude
, HTTP.Manager
, Man.ReqMan, Man.mkReqMan
, module X
)
where

import MyPrelude as X hiding (Selector)
import Test.QuickCheck                as X
import Test.Hspec                as X
import Network.Haskoin.Test       as X

import Servant.Client as X
import qualified RBPCP.Internal.Manager     as Man
import qualified Servant.Client             as SC
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Types.Status  as Stat
import Control.Exception                    (throw)
import Data.Function                        (fix)
import Control.Concurrent                   (threadDelay)
import Control.Monad                        (unless)
import qualified PaymentChannel.Test        as PC
import Data.Ratio
import qualified RBPCP.Api                      as API
import qualified RBPCP.Types                    as RBPCP



mkRbpcpPayment :: PC.SignedPayment -> RBPCP.Payment
mkRbpcpPayment p = RBPCP.Payment (PC.toPaymentData p) ""

-- rbpcp-api client functions
fundingInfo
    :: RBPCP.Client RBPCP.PubKey
    -> RBPCP.BLT
    -> SC.ClientM RBPCP.FundingInfo
fundingInfo = SC.client (Proxy :: Proxy API.FundInfo)

chanOpen :: RBPCP.BtcTxId
         -> RBPCP.Vout
         -> Maybe RBPCP.SharedSecret
         -> RBPCP.Payment
         -> SC.ClientM RBPCP.PaymentResult
chanOpen = SC.client (Proxy :: Proxy API.ChanOpen)

chanPay :: RBPCP.BtcTxId
        -> RBPCP.Vout
        -> Maybe RBPCP.SharedSecret
        -> RBPCP.Payment
        -> SC.ClientM RBPCP.PaymentResult
chanPay = SC.client (Proxy :: Proxy API.ChanPay)

chanClose :: RBPCP.BtcTxId
          -> RBPCP.Vout
          -> Maybe RBPCP.SharedSecret
          -> RBPCP.Payment
          -> SC.ClientM RBPCP.PaymentResult
chanClose = SC.client (Proxy :: Proxy API.ChanClose)


divRatio :: (Real a, Real b) => a -> b -> Ratio Word64
divRatio a b = (realToFrac a :: Ratio Word64) / (realToFrac b :: Ratio Word64)

runReq :: (BaseUrl, Man.ReqMan) -> SC.ClientM a -> IO (Either SC.ServantError a)
runReq (baseUrl, man) =
    Man.runServantClient man baseUrl

testRunReq :: (BaseUrl, Man.ReqMan) -> String -> SC.ClientM a -> IO a
testRunReq (baseUrl, man) descr req =
    either (\e -> error $ descr ++ " request fail: " ++ show e) return
      =<< runReq (baseUrl, man) req
