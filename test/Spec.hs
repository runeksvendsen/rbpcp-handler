module Main where

import MyPrelude
import Network.Haskoin.Test
import Test.Hspec
import Test.QuickCheck
import Servant
import Servant.API
import Servant.Client

import qualified RBPCP.Api                    as API
import qualified RBPCP.Types                  as RBPCP
import qualified Network.Haskoin.Crypto       as HC


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Hey ho" $
        it "Woot" $ do
            ArbitraryPubKeyC prvKey pubKey <- generate arbitrary


            1 `shouldBe` 1



fundingInfo :: RBPCP.Client RBPCP.PubKey -> RBPCP.BLT -> ClientM RBPCP.FundingInfo
fundingInfo = client (Proxy :: Proxy API.FundInfo)

chanOpen :: RBPCP.BtcTxId -> RBPCP.Vout -> Maybe HC.Hash256 -> RBPCP.Payment -> ClientM RBPCP.PaymentResult
chanOpen = client (Proxy :: Proxy API.ChanOpen)

chanPay :: RBPCP.BtcTxId -> RBPCP.Vout -> Maybe HC.Hash256 -> RBPCP.Payment -> ClientM RBPCP.PaymentResult
chanPay = client (Proxy :: Proxy API.ChanPay)

chanClose :: RBPCP.BtcTxId -> RBPCP.Vout -> Maybe HC.Hash256 -> RBPCP.Payment -> ClientM RBPCP.PaymentResult
chanClose = client (Proxy :: Proxy API.ChanClose)
