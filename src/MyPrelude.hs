module MyPrelude
( module MyPrelude
, Text
, cs
, module X
, R.liftIO
, fromString
, Status(..)
, HTTP.Manager, HTTP.newManager, tlsManagerSettings
, HT.TxHash
)
where

import Data.String (fromString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word as X (Word32, Word64)
import Data.Maybe as X
import Control.Monad as X
import Data.Monoid   as X
import qualified Control.Monad.Reader as R
import Network.HTTP.Types.Status (Status(..))
import GHC.Generics as X
import Text.Printf as X
import Data.Function as X
import Control.Concurrent as X
import Data.Proxy as X
import qualified Network.Haskoin.Transaction    as HT

-- runReq
import qualified Servant.Client as SC
import           Data.EitherR               (fmapL)
import           Servant.Common.BaseUrl     as X
import qualified Network.HTTP.Client        as HTTP
import Network.HTTP.Client.TLS              (tlsManagerSettings)
import Control.Monad.Catch            as X

type BlockNumber = Word32

data Error
  = UserError String
  | InternalError InternalError
        deriving (Eq, Show)

data InternalError
  = RequestError BaseUrl SC.ServantError
  | OtherInternalErr String
      deriving (Eq, Show)

instance Exception Error


runReq :: (BaseUrl, HTTP.Manager) -> SC.ClientM a -> IO a
runReq (baseUrl, man) req =
        SC.runClientM req clientEnv
    >>= either (throwM . InternalError . RequestError baseUrl) return
  where
    clientEnv = SC.ClientEnv man baseUrl



