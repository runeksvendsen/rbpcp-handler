{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module MyPrelude
( module MyPrelude
, Text
, cs
, module X
, fromString
, Status(..)
, HTTP.Manager, HTTP.newManager, tlsManagerSettings
, HT.TxHash
, Log.runStdoutLoggingT
, PC.PayChanError
)
where

import RBPCP.Internal.Manager

import Data.String (fromString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word as X (Word32, Word64)
import Data.Maybe as X
import Control.Monad as X
import Data.Monoid   as X
import Network.HTTP.Types.Status (Status(..))
import GHC.Generics as X
import Text.Printf as X
import Data.Function as X
import Control.Concurrent as X
import Data.Proxy as X
import Control.Monad.IO.Class as X
import Data.EitherR as X
import Data.Tagged as X
import qualified Network.Haskoin.Transaction    as HT
import qualified Network.Haskoin.Crypto         as HC
import qualified PaymentChannel               as PC
import qualified ChanDB                       as DB
import qualified Control.Monad.Logger as Log
import qualified Data.Text as T
import Data.Aeson -- (FromJSON, ToJSON, encode)
import qualified Data.Serialize     as Bin
import System.Environment (lookupEnv)
import Text.Read (readMaybe)


import Control.Monad.Time as X

import qualified Servant.Server as SS
import qualified Servant.Client as SC
import           Data.EitherR               (fmapL)
import           Servant.Common.BaseUrl     as X
import qualified Network.HTTP.Client        as HTTP
import Network.HTTP.Client.TLS              (tlsManagerSettings)
import Control.Monad.Catch            as X
import Control.Monad.Except     as X
-- runReq

import qualified RBPCP.Types                  as RBPCP
import qualified RBPCP.Api                    as API
import qualified Web.HttpApiData              as Web
import qualified Servant.Utils.Links          as SL
import qualified Data.ByteString.Base16       as B16
import Control.Monad.Reader as X
import Control.Monad.Trans.Either as X
import Control.Monad.Base         as X


type BitcoinTx = HT.Tx

--type PubKeyGetter = IO (Either PubKeyDbException DB.KeyAtIndex)

--newtype PubKeyDbException = PubKeyDbException DB.ChanDBException
--    deriving (Eq, Show)

showJsonStr :: ToJSON a => a -> String
showJsonStr = cs . encode


-- |We use this monad for the handlers, which gives them access to configuration data
--  of type 'conf'.
type AppM conf = ReaderT conf SS.Handler


-- |Transform an 'AppM conf' into a 'Servant.Handler'
readerToEither :: conf -> AppM conf SS.:~> SS.Handler
readerToEither cfg = SS.Nat $ \x -> runReaderT x cfg

envRead :: Read a => String -> IO (Maybe a)
envRead envVar = maybe Nothing readMaybe <$> lookupEnv envVar

mkUrl :: RBPCP.BtcTxId -> Word32 -> RBPCP.SharedSecret -> T.Text
mkUrl h i s = ("/" <>) . cs . show $ SL.safeLink api endPoint h i (Just s)
    where api :: Proxy API.RBPCP
          api = Proxy
          endPoint :: Proxy API.ChanPay -- Same as API.ChanOpen
          endPoint = Proxy


newTlsManager :: IO HTTP.Manager
newTlsManager = HTTP.newManager tlsManagerSettings


instance Web.FromHttpApiData HC.PubKeyC where
    parseUrlPiece = fmapL cs . Bin.decode . fst . B16.decode . cs
instance Web.ToHttpApiData HC.PubKeyC where
    toUrlPiece = cs . B16.encode . Bin.encode

