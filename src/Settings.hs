module Settings where

import MyPrelude
import PaymentChannel.Internal.Receiver.Key
import qualified Network.Haskoin.Crypto       as HC
import qualified Servant.Client               as SC
import qualified PaymentChannel               as PC


-- | The server's channel public keys are derived from this key.
--   Every time a client opens a channel, the next key is handed out to new clients.
confServerExtPub :: External ChildPub
confServerExtPub = fromXPub $ fromMaybe (error "Bad server XPubKey") $ HC.xPubImport 
     "xpub6CWiJoiwxPQni3DFbrQNHWq8kwrL2J1HuBN7zm4xKPCZRmEshc7Dojz4zMah7E4o2GEEbD6HgfG7sQid186Fw9x9akMNKw2mu1PjqacTJB2"

confMinBtcConf :: Word
confMinBtcConf = 6

confOpenPrice :: PC.BtcAmount
confOpenPrice = 30000

confMinDurationHours :: Word
confMinDurationHours = 48

confProofServer :: SC.BaseUrl
confProofServer = SC.BaseUrl SC.Https "blockchain.runeks.me" 443 ""

confBitcoinSigner :: SC.BaseUrl
confBitcoinSigner = SC.BaseUrl SC.Http "localhost" 8081 ""

spvWalletCacheDir :: Text
spvWalletCacheDir = "/Users/rune/.btc-spv/"