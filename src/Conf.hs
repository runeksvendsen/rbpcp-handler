module Conf where

import MyPrelude
import qualified Bitcoin.SPV.Wallet.Interface      as Wall
import qualified Control.Monad.Reader as Reader
import qualified Network.HTTP.Client        as HTTP



data HandlerConf chanDb = HandlerConf
    { hcSpvWallet   :: !Wall.Interface
    , hcChanDb      :: !chanDb
    , hcCurrPubKey  :: !PubKeyGetter
    , hcManager     :: !HTTP.Manager
    }



