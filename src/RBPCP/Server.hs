{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module RBPCP.Server
( createServer
, module X
)
where

import RBPCP.Handler.FundingInfo  as X
import RBPCP.Handler.Open         as X
import RBPCP.Handler.Pay          as X
import RBPCP.Handler.Close        as X


import MyPrelude
import RBPCP.Handler.Internal.Util
import qualified RBPCP.Types                  as RBPCP
import           Servant
import qualified ChanDB                       as DB
import qualified RBPCP.Api                    as Api


-- | Create server, specifying database type and function which is called for every payment received
createServer :: forall txM dbM dbH.
    ( DB.ChanDBTx txM dbM dbH )
    => Proxy (txM ())
    -> PaymentCallback
    -> ServerT Api.RBPCP (HandlerM dbH)
createServer _ payCb =
    fundingInfo :<|> open :<|> pay :<|> close
  where
    open  tid vout s p = runOpen
        (openE tid vout s p ::
            ReaderT (HandlerConf dbH) (EitherT (HandlerErr OpenErr) dbM) RBPCP.PaymentResult)
    pay   tid vout s p = runPay payCb
        (payE tid vout s p ::
            ReaderT PaymentCallback (EitherT (HandlerErr PaymentError) txM) RBPCP.PaymentResult)
    close tid vout s p = runClose
        (closeE tid vout s p ::
            ReaderT (HandlerConf dbH) (EitherT (HandlerErr PayChanError) txM) RBPCP.PaymentResult)

