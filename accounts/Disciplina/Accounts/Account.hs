
module Disciplina.Accounts.Account
       ( module Disciplina.Accounts.Account
       ) where

import Universum

import Control.Lens

data Account hash = Account
    { _aBalance :: Integer
    , _aNonce   :: Int
    , _aStorage :: hash
    , _aCode    :: hash
    }

makeLenses ''Account

