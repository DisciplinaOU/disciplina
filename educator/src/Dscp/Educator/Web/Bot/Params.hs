module Dscp.Educator.Web.Bot.Params
    ( EducatorBotParams (..)
    , ebpEnabledL
    , ebpSeedL
    , ebpOperationsDelayL
    ) where

import Control.Lens (makeLensesWith)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)
import Time.Units (Microsecond, Time)

import Dscp.Util (postfixLFields)
import Dscp.Util.Aeson ()

-- | Which params to use when launching bot.
-- Bool flag is used instead of `Maybe` value or custom sum type
-- to allow for specifying default values for bot params in
-- default config even though bot should be disabled by default.
data EducatorBotParams = EducatorBotParams
    { ebpEnabled         :: !Bool
      -- ^ Whether or not the bot is enabled
    , ebpSeed            :: !Text
      -- ^ Seed to generate initial data (assignments, ...).
    , ebpOperationsDelay :: !(Time Microsecond)
      -- ^ Artificial delay in bot operations.
    } deriving (Show, Eq)

makeLensesWith postfixLFields ''EducatorBotParams
deriveFromJSON defaultOptions ''EducatorBotParams
