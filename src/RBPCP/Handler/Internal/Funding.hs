module RBPCP.Handler.Internal.Funding
( fundingProofM
, unspentOuts
, AddressFundingInfo(..)
)
where

import MyPrelude
import Network.HTTP.Types.Status                  (Status(..))
import Servant.Client
import Network.Bitcoin.AddrIndex.API              ( PublishTx, UnspentOuts, TxOutProof
                                                  , Addr(..), PushTxReq(..)
                                                  , AddressFundingInfo(..), FundingProof
                                                  )

import qualified Network.Haskoin.Transaction      as HT
import qualified Control.Monad.Catch              as Catch
import qualified Network.Haskoin.Crypto           as HC


unspentOuts :: HC.Address -> ClientM [AddressFundingInfo]
unspentOuts = client api . Addr
    where api :: Proxy UnspentOuts
          api  = Proxy

txOutProof :: HT.TxHash -> ClientM FundingProof
txOutProof = client api
    where api :: Proxy TxOutProof
          api  = Proxy

fundingProofM :: HT.TxHash -> ClientM (Maybe FundingProof)
fundingProofM = maybe404 . client api
       where api :: Proxy TxOutProof
             api = Proxy

maybe404 :: ClientM a -> ClientM (Maybe a)
maybe404 cm = Catch.try cm >>= getRes
   where getRes (Right a) = return $ Just a
         getRes (Left (FailureResponse (Status 404 _) _ _)) = return Nothing
         getRes (Left ex) = Catch.throwM (ex :: ServantError)
