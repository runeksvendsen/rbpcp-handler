{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
( getConfProof
, ConfirmationInfo(..)
, verifyFundingProof
)
where

import MyPrelude

import  qualified
       Network.Haskoin.Transaction    as HT
import  qualified
       Network.Haskoin.Crypto         as HC
import  qualified
       Network.Haskoin.Block          as HB
import  qualified
       Control.Monad.Catch            as Catch
import Servant
import Servant.Client
import Network.Bitcoin.AddrIndex.API  ( TxOutProof
--                                      , UnspentOuts
--                                      , AddressFundingInfo(..)
                                      , FundingProof(..)
                                      , verifyFundingProof
                                      )
import Bitcoin.SPV.Wallet             ( Interface
                                      , ConfirmationInfo(..)
                                      , blockConfInfo
                                      )
import qualified Bitcoin.SPV.Wallet.Extra     as Lib

