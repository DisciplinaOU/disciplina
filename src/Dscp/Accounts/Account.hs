
{-# LANGUAGE DeriveAnyClass #-}

module Dscp.Accounts.Account where

import Universum

import Codec.Serialise (Serialise)
import Control.Lens (makeLenses)
import Data.Default (Default)

data Account hash = Account
    { _aBalance :: Amount
    , _aNonce   :: Int
    , _aStorage :: hash
    , _aCode    :: hash
    }
    deriving (Show, Eq, Generic, Serialise, Default)

type Amount = Integer

makeLenses ''Account

