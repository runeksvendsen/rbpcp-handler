{-# LANGUAGE TypeOperators, TemplateHaskell, ScopedTypeVariables #-}

module Conf
( module Conf
, module RBPCP.Internal.Manager
)
where

import MyPrelude
import RBPCP.Internal.Manager
import qualified Bitcoin.SPV.Wallet.Interface      as Wall
import qualified Network.HTTP.Client        as HTTP
--

import qualified Settings

import qualified Bitcoin.SPV.Wallet           as Wall

import           Servant
import qualified Network.Wai as Wai
import qualified ChanDB                       as DB
import qualified RBPCP.Api                    as Api
import qualified Servant.Server               as SS
import qualified Control.Monad.Reader        as Reader
import qualified Control.Monad.Logger        as Log
import qualified Network.Wai.Handler.Warp   as Warp
import qualified System.IO as IO
import qualified System.ZMQ4                  as ZMQ

import qualified Database.Persist.Sqlite      as Sqlite



data HandlerConf chanDb = HandlerConf
    { hcSpvWallet   :: !Wall.Interface
    , hcChanDb      :: !chanDb
    , hcCurrPubKey  :: !PubKeyGetter
    , hcManager     :: !ReqMan
    }


instance Monad m => HasReqMan (Reader.ReaderT (HandlerConf a) m) where
    getReqMan = Reader.asks hcManager

