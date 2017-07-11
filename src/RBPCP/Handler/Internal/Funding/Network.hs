module RBPCP.Handler.Internal.Funding.Network
( HCC.prodnet
, HCC.testnet3
, disabledNet
)
where

import qualified Network.Haskoin.Constants            as HCC


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
