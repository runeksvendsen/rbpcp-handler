module RBPCP.Handler.Internal.Funding.Network
( BtcNetwork(..)
, Prodnet(..)
, Testnet3(..)
, DisabledNet(..)
)
where

import qualified Network.Haskoin.Constants            as HCC
--import Control.Applicative ((<|>))


class (Eq net, Show net) => BtcNetwork net where
    fromHaskoinNet :: HCC.Network -> Maybe net

data Prodnet = Prodnet          deriving (Eq, Show)
data Testnet3 = Testnet3        deriving (Eq, Show)
data DisabledNet = DisabledNet  deriving (Eq, Show)

instance BtcNetwork Prodnet where
    fromHaskoinNet net = if net == HCC.prodnet then Just Prodnet else Nothing
instance BtcNetwork Testnet3 where
    fromHaskoinNet net = if net == HCC.testnet3 then Just Testnet3 else Nothing
instance BtcNetwork DisabledNet where
    fromHaskoinNet net = if net == disabledNet then Just DisabledNet else Nothing

--toHaskoinNet :: BtcNet -> HCC.Network
--toHaskoinNet Prodnet    = HCC.prodnet
--toHaskoinNet Testnet3   = HCC.testnet3
--toHaskoinNet DisabledNet = disabledNet

-- | A Bitcoin 'HCC.Network' which signifies that we don't reach out
--    to the Bitcoin network at all (used for testing)
disabledNet :: HCC.Network
disabledNet = HCC.testnet3
    { HCC.getNetworkName      = "disabled"
    , HCC.getAddrPrefix       = 86
    , HCC.getScriptPrefix     = 24
    , HCC.getSecretPrefix     = 113
    , HCC.getExtPubKeyPrefix  = 0x04170786
    , HCC.getExtSecretPrefix  = 0x04180786
    , HCC.getNetworkMagic     = 0xdeadbeef
    , HCC.getMaxBlockSize     = 1000000
    , HCC.getMaxSatoshi       = 2100000000000000
    , HCC.getHaskoinUserAgent = ""
    , HCC.getDefaultPort      = 0
    }
