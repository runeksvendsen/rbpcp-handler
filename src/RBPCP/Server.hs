{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module RBPCP.Server
( server )
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



server :: forall txM dbM dbH.
    ( DB.ChanDB dbM dbH
    , DB.ChanDBTx txM dbM dbH
    )
    => Proxy (txM ())
    -> ServerT Api.RBPCP (HandlerM dbH)
server _ =
    fundingInfo :<|> open :<|> pay :<|> close
  where
    close tid vout s p = runClose
        (closeE tid vout s p ::
            ReaderT (HandlerConf dbH) (EitherT (HandlerErr PayChanError) txM) RBPCP.PaymentResult)
    pay   tid vout s p = runAtomic
        (payE tid vout s p  ::
            EitherT (HandlerErr PayChanError) txM RBPCP.PaymentResult)
    open  tid vout s p = runOpen
        (openE tid vout s p :: ReaderT (HandlerConf dbH) (EitherT (HandlerErr OpenErr) dbM) RBPCP.PaymentResult)


