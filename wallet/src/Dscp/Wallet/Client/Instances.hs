module Dscp.Wallet.Client.Instances () where

import Servant.API (ToHttpApiData (..))

import Dscp.Core

instance ToHttpApiData Address where
    toUrlPiece = addrToText
