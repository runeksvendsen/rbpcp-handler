{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RBPCP.Handler.Internal.Error where

import MyPrelude
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import qualified Network.Haskoin.Crypto         as HC
import qualified PaymentChannel               as PC
import qualified Data.Text as T
import qualified RBPCP.Types                    as RBPCP
import qualified Servant.Client             as SC
import qualified Servant.Server       as SS


camelToUnderstore :: Options
camelToUnderstore =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_'
    }

instance FromJSON UserError where
  parseJSON  = genericParseJSON  camelToUnderstore
instance ToJSON UserError where
  toJSON     = genericToJSON     camelToUnderstore


data UserError
  = ResourceNotFound          -- 404
  | ResourcePaymentMismatch RBPCP.BtcTxId Word32 RBPCP.SharedSecret  -- redirect
        deriving (Generic, Eq, Show)

class HasErrorResponse e where
    errRes :: e -> T.Text

-- Handler error response
newtype HandlerErrorMsg a = HandlerErrorMsg
    { error_msg :: ErrorResp a
    } deriving (Eq, ToJSON)

newtype ErrorResp a = ErrorResp { errorResp :: a } deriving (Eq)
instance HasErrorResponse a => ToJSON (ErrorResp a) where
    toJSON = String . errRes . errorResp

instance HasErrorResponse PC.PayChanError where
    errRes = cs . show

instance HasErrorResponse InternalError where
    errRes = const "Internal Error"

mkServantErr :: HasErrorResponse e => SS.ServantErr -> e -> SS.ServantErr
mkServantErr se pce = se { SS.errBody = cs . encode . HandlerErrorMsg $ ErrorResp pce }

throwUserError :: (HasErrorResponse e, MonadError SS.ServantErr m) => e -> m a
throwUserError = throwError . mkServantErr SS.err400

throwServerError :: MonadError SS.ServantErr m => InternalError -> m a
throwServerError = throwError . mkServantErr SS.err500



