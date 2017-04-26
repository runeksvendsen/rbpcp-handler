module RBPCP.Handler.Internal.Types where

import MyPrelude
import Conf
import qualified ChanDB         as DB

import qualified RBPCP.Types                  as RBPCP
import qualified RBPCP.Api                    as API
import qualified Web.HttpApiData              as Web
import qualified Servant.Utils.Links          as SL
import qualified Bitcoin.SPV.Wallet           as Wall
import qualified Data.ByteString.Base16       as B16
import qualified Control.Monad.Reader         as Reader
import qualified Servant.Client               as SC
import qualified Servant.Server               as SS
import qualified PaymentChannel               as PC



type HandlerM dbConf = AppM (HandlerConf dbConf)

class HasSpvWallet m where
    wallIface :: m Wall.Interface

instance HasSpvWallet (HandlerM dbH) where
    wallIface = Reader.asks hcSpvWallet

type BlockNumber = Word32


data HandlerErr a
  = HandlerErr a
  | UserError UserError
  | InternalErr InternalError
      deriving (Eq, Show)

toHandlerEx :: DB.ChanDBException -> HandlerErr a
toHandlerEx e =
        if DB.is404 e
            then UserError ResourceNotFound
            else InternalErr $ OtherInternalErr "DB Error"

class IsHandlerException e where
    mkHandlerErr :: e -> SS.ServantErr

instance IsHandlerException a => IsHandlerException (HandlerErr a) where
    mkHandlerErr (HandlerErr e) = mkHandlerErr e
    mkHandlerErr (InternalErr _) = mkServantErr SS.err500 (OtherInternalErr "")
    mkHandlerErr (UserError e) = mkHandlerErr e

instance IsHandlerException DB.ChanDBException where
    mkHandlerErr e =
        if DB.is404 e
            then SS.err404
            else mkServantErr SS.err500 (OtherInternalErr "")


instance IsHandlerException PC.PayChanError where
    mkHandlerErr = mkServantErr SS.err400

instance IsHandlerException PubKeyDbException where
    mkHandlerErr _ = mkServantErr SS.err500 (OtherInternalErr "")

instance IsHandlerException UserError where
    mkHandlerErr ResourceNotFound = SS.err404
    mkHandlerErr (ResourcePaymentMismatch txid vout s) = SS.err301
         { SS.errHeaders = [ ("Location", cs $ mkUrl txid vout s) ]
         , SS.errHTTPCode = 308   -- repeat request of same type (POST/PUT/etc.) on new URL
         }

instance IsHandlerException InternalError where
    mkHandlerErr = mkServantErr SS.err500

class HasHandlerConf m chanDB where
    handlerConf :: m (HandlerConf chanDB)

instance HasHandlerConf (AppM (HandlerConf chanDb)) chanDb where
    handlerConf = Reader.ask

class HasDbConf m chanDb where
    getDbConf :: m chanDb

instance HasDbConf (AppM (HandlerConf chanDb)) chanDb where
    getDbConf = Reader.asks hcChanDb

class HasHttpManager m where
    getManager :: m Manager
