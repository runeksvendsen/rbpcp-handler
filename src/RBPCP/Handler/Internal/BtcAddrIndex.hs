module RBPCP.Handler.Internal.BtcAddrIndex
( AddressFundingInfo(..)
, unspentOuts
, fetchBtcTxM
, publishTx
)
where

import MyPrelude
import Network.Bitcoin.AddrIndex.API              ( PublishTx, UnspentOuts, TxOutProof
                                                  , Addr(..), PushTxReq(..)
                                                  , AddressFundingInfo(..), FundingProof(..)
                                                  )
import Network.HTTP.Types.Status                  (Status(..))
import Servant.Client
import qualified Control.Monad.Catch              as Catch
import qualified Network.Haskoin.Transaction      as HT
import qualified Network.Haskoin.Crypto           as HC


unspentOuts :: HC.Address -> ClientM [AddressFundingInfo]
unspentOuts = client api . Addr
    where api :: Proxy UnspentOuts
          api  = Proxy

fetchBtcTxM :: HT.TxHash -> ClientM (Maybe HT.Tx)
fetchBtcTxM h = maybe404 $ proof_tx_data <$> client api h
       where api :: Proxy TxOutProof
             api = Proxy

publishTx :: BitcoinTx -> ClientM ()
publishTx = void . client api . PushTxReq
    where api :: Proxy PublishTx
          api  = Proxy

maybe404 :: ClientM a -> ClientM (Maybe a)
maybe404 cm = Catch.try cm >>= getRes
   where getRes (Right a) = return $ Just a
         getRes (Left (FailureResponse (Status 404 _) _ _)) = return Nothing
         getRes (Left ex) = Catch.throwM (ex :: ServantError)
