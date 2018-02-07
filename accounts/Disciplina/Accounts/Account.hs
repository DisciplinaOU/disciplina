
module Disciplina.Accounts.Account
       ( module Disciplina.Accounts.Account
       ) where

import Universum

import Control.Lens

data Account storageHash codeHash = Account
    { _aBalance :: Integer
    , _aNonce   :: Int
    , _aStorage :: storageHash
    , _aCode    :: codeHash
    }

makeLenses ''Account

