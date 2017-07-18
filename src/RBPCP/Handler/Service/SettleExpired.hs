module RBPCP.Handler.Service.SettleExpired where

import qualified PaymentChannel   as PC
import qualified ChanDB           as DB
import qualified Data.Time.Clock            as Time
import qualified Data.Time.Clock.POSIX    as Time
import           Control.Monad.Time       as Time


-- | Settle channels close to expiration by publishing
--    a transaction that moves funds from the mutual,
--    time-locked address to client's + server's address
settleExpired
    :: (MonadTime m, DB.ChanDB m cfg)
    => PC.Hour
    -- ^ Settlement period. How many hours before actual,
    -- in-blockchain expiration date do we begin settlement?
    -> m ()
settleExpired settlePeriod = do
    now <- Time.currentTime
    let cutoffTime = Time.posixSecondsToUTCTime $ Time.utcTimeToPOSIXSeconds now - PC.toSeconds settlePeriod
    expiredKeys <- DB.selectChannels (DB.ExpiringBefore cutoffTime)



    return ()


